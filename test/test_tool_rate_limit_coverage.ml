(** Coverage tests for Tool_rate_limit *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_rate_limit Coverage Tests ===\n"

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "✓ %s passed\n" name
  with e ->
    Printf.printf "✗ %s FAILED: %s\n" name (Printexc.to_string e);
    exit 1

(* Create test context *)
let test_counter = ref 0
let make_test_ctx () =
  incr test_counter;
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-rate-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let registry = Session.create () in
  { Tool_rate_limit.config; agent_name = "test-agent"; registry }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_rate_limit.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test rate_limit_status dispatch - needs Eio for Session.get_rate_limit_status *)
let () = test "dispatch_rate_limit_status" (fun () ->
  Eio_main.run @@ fun _env ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_rate_limit.dispatch ctx ~name:"masc_rate_limit_status" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_rate_limit_status - needs Eio *)
let () = test "handle_rate_limit_status" (fun () ->
  Eio_main.run @@ fun _env ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_rate_limit.handle_rate_limit_status ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test rate_limit_config dispatch *)
let () = test "dispatch_rate_limit_config" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_rate_limit.dispatch ctx ~name:"masc_rate_limit_config" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_rate_limit_config *)
let () = test "handle_rate_limit_config" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_rate_limit.handle_rate_limit_config ctx args in
  assert success;
  assert (String.length result > 0)
)

let () = Printf.printf "\n✅ All Tool_rate_limit tests passed!\n"
