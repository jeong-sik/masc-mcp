(** Multi-Room Tests - TDD for room management *)

open Alcotest
open Masc_mcp

(* Test helpers *)
let setup_test_room () =
  (* Use PID + timestamp for deterministic unique dir *)
  let test_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_multi_room_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir test_dir 0o755;
  let config = Room.default_config test_dir in
  (config, test_dir)

let cleanup_test_room test_dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" test_dir))

let task_count config =
  let backlog = Room.read_backlog config in
  List.length backlog.tasks

let extract_nickname result =
  let prefix = "  Nickname: " in
  try
    let start_idx =
      let idx = ref 0 in
      while !idx < String.length result - String.length prefix &&
            String.sub result !idx (String.length prefix) <> prefix do
        incr idx
      done;
      !idx + String.length prefix
    in
    let end_idx = String.index_from result start_idx '\n' in
    String.sub result start_idx (end_idx - start_idx)
  with _ -> "unknown-agent"

(* ============================================ *)
(* Room Registry Tests                          *)
(* ============================================ *)

let test_rooms_list_empty () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    (* init 전에는 빈 목록 *)
    let result = Room.rooms_list config in
    match result with
    | `Assoc fields ->
        let rooms = List.assoc_opt "rooms" fields in
        (match rooms with
        | Some (`List []) -> ()
        | Some (`List _) -> fail "Expected empty room list before init"
        | _ -> fail "Expected rooms field")
    | _ -> fail "Expected JSON object"
  )

let test_rooms_list_after_init () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    (* init 하면 default 방이 생성됨 *)
    let _ = Room.init config ~agent_name:None in
    let result = Room.rooms_list config in
    match result with
    | `Assoc fields ->
        let rooms = List.assoc_opt "rooms" fields in
        (match rooms with
        | Some (`List rooms_list) ->
            check bool "has at least one room" true (List.length rooms_list >= 1);
            (* default 방이 있어야 함 *)
            let has_default = List.exists (fun r ->
              match r with
              | `Assoc r_fields ->
                  (match List.assoc_opt "id" r_fields with
                  | Some (`String id) -> id = "default"
                  | _ -> false)
              | _ -> false
            ) rooms_list in
            check bool "has default room" true has_default
        | _ -> fail "Expected rooms list")
    | _ -> fail "Expected JSON object"
  )

(* ============================================ *)
(* Room Create Tests                            *)
(* ============================================ *)

let test_room_create () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    let _ = Room.init config ~agent_name:None in
    (* 새 방 생성 *)
    let result = Room.room_create config ~name:"My Project Dev" ~description:(Some "Development room") in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "id" fields with
        | Some (`String id) ->
            check bool "id is slugified" true (id = "my-project-dev")
        | _ -> fail "Expected id field");
        (match List.assoc_opt "error" fields with
        | Some _ -> fail "Unexpected error"
        | None -> ())
    | _ -> fail "Expected JSON object"
  )

let test_room_create_duplicate () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    let _ = Room.init config ~agent_name:None in
    (* 첫 번째 생성 *)
    let _ = Room.room_create config ~name:"Test Room" ~description:None in
    (* 중복 생성 시도 *)
    let result = Room.room_create config ~name:"Test Room" ~description:None in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "error" fields with
        | Some (`String err) -> check bool "error contains exists" true (String.length err > 0)
        | _ -> fail "Expected error for duplicate room")
    | _ -> fail "Expected JSON object"
  )

(* ============================================ *)
(* Room Enter Tests                             *)
(* ============================================ *)

let test_room_enter () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    let _ = Room.init config ~agent_name:None in
    (* 새 방 생성 *)
    let _ = Room.room_create config ~name:"Other Room" ~description:None in
    (* 방 입장 *)
    let result = Room.room_enter config ~room_id:"other-room" ~agent_type:"claude" () in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "current_room" fields with
        | Some (`String room) -> check string "entered room" "other-room" room
        | _ -> fail "Expected current_room field");
        (* 닉네임이 부여되어야 함 *)
        (match List.assoc_opt "nickname" fields with
        | Some (`String nick) -> check bool "has nickname" true (String.length nick > 0)
        | _ -> fail "Expected nickname field")
    | _ -> fail "Expected JSON object"
  )

let test_room_enter_nonexistent () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    let _ = Room.init config ~agent_name:None in
    (* 존재하지 않는 방 입장 시도 *)
    let result = Room.room_enter config ~room_id:"nonexistent" ~agent_type:"claude" () in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "error" fields with
        | Some _ -> ()  (* 에러가 있어야 함 *)
        | None -> fail "Expected error for nonexistent room")
    | _ -> fail "Expected JSON object"
  )

(* ============================================ *)
(* Room Context Tests                           *)
(* ============================================ *)

let test_current_room_after_enter () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    let _ = Room.init config ~agent_name:None in
    (* 새 방 생성하고 입장 *)
    let _ = Room.room_create config ~name:"New Room" ~description:None in
    let _ = Room.room_enter config ~room_id:"new-room" ~agent_type:"claude" () in
    (* rooms_list에서 current_room 확인 *)
    let result = Room.rooms_list config in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "current_room" fields with
        | Some (`String room) -> check string "current room" "new-room" room
        | _ -> fail "Expected current_room field")
    | _ -> fail "Expected JSON object"
  )

(* ============================================ *)
(* Room Isolation Tests                         *)
(* ============================================ *)

let test_room_enter_moves_agent () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    ignore (Room.init config ~agent_name:None);

    let join_result = Room.join config ~agent_name:"codex" ~capabilities:[] () in
    let nickname = extract_nickname join_result in
    check int "default room has agent" 1 (Room.count_agents_in_room config "default");

    ignore (Room.room_create config ~name:"Move Room" ~description:None);
    ignore (Room.room_enter config ~room_id:"move-room" ~agent_type:"codex" ~agent_name:nickname ());

    check int "default room cleared after move" 0 (Room.count_agents_in_room config "default");
    check int "move room has agent" 1 (Room.count_agents_in_room config "move-room");
  )

let test_room_task_isolation () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    ignore (Room.init config ~agent_name:None);

    ignore (Room.add_task config ~title:"default-task" ~priority:3 ~description:"default room task");
    check int "default room task count" 1 (task_count config);

    ignore (Room.room_create config ~name:"Isolated Room" ~description:None);
    ignore (Room.room_enter config ~room_id:"isolated-room" ~agent_type:"codex" ());

    (* Default room tasks should not leak into the new room. *)
    check int "isolated room starts empty" 0 (task_count config);

    ignore (Room.add_task config ~title:"isolated-task" ~priority:3 ~description:"isolated room task");
    check int "isolated room task count" 1 (task_count config);

    ignore (Room.room_enter config ~room_id:"default" ~agent_type:"codex" ());
    check int "default room remains isolated" 1 (task_count config);
  )

(* ============================================ *)
(* Backward Compatibility Tests                 *)
(* ============================================ *)

let test_legacy_masc_as_default_room () =
  let (config, test_dir) = setup_test_room () in
  Fun.protect ~finally:(fun () -> cleanup_test_room test_dir) (fun () ->
    (* Legacy 스타일로 init (rooms/ 없이) *)
    let _ = Room.init config ~agent_name:None in
    (* join이 default 방에서 작동해야 함 *)
    let join_result = Room.join config ~agent_name:"claude" ~capabilities:[] () in
    check bool "join succeeded" true (String.length join_result > 0);
    (* join_result에 nickname이 포함되어 있어야 함 (claude-xxx 형태) *)
    check bool "join result contains claude" true
      (String.length join_result > 10 &&
       String.sub join_result 0 6 = "\xE2\x9C\x85 " || (* ✅ *)
       (let contains s sub =
         let len_s = String.length s in
         let len_sub = String.length sub in
         if len_sub > len_s then false
         else
           let rec check i =
             if i > len_s - len_sub then false
             else if String.sub s i len_sub = sub then true
             else check (i + 1)
           in check 0
       in contains join_result "claude-"))
  )

(* ============================================ *)
(* Test Suite                                   *)
(* ============================================ *)

let () =
  run "Multi-Room" [
    "rooms_list", [
      test_case "empty before init" `Quick test_rooms_list_empty;
      test_case "default room after init" `Quick test_rooms_list_after_init;
    ];
    "room_create", [
      test_case "create new room" `Quick test_room_create;
      test_case "duplicate room error" `Quick test_room_create_duplicate;
    ];
    "room_enter", [
      test_case "enter room" `Quick test_room_enter;
      test_case "enter nonexistent error" `Quick test_room_enter_nonexistent;
      test_case "current room updates" `Quick test_current_room_after_enter;
    ];
    "room_isolation", [
      test_case "agent moves between rooms" `Quick test_room_enter_moves_agent;
      test_case "tasks are isolated per room" `Quick test_room_task_isolation;
    ];
    "backward_compat", [
      test_case "legacy masc as default" `Quick test_legacy_masc_as_default_room;
    ];
  ]
