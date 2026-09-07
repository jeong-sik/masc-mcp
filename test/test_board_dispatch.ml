(** Test Board_dispatch - routing and JSONL backend integration *)

open Masc

let () = Mirage_crypto_rng_unix.use_default ()
let () = Random.self_init ()

(** Temp directory for test isolation — set before any Board.global call *)
let fresh_test_base_path () =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "masc-test-board-dispatch-%06x" (Random.bits ()))
  in
  Unix.putenv "MASC_BASE_PATH" dir;
  dir

(** Wrap test body in Eio runtime with isolated JSONL backend *)
let with_eio f () =
  Eio_main.run @@ fun env ->
  Fs_compat.set_fs (Eio.Stdenv.fs env);
  ignore (fresh_test_base_path ());
  Board.reset_global_for_test ();
  Board_dispatch.reset_for_test ();
  Board_dispatch.init_jsonl ();
  f ()

let block_board_masc_dir_with_file () =
  let base =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "masc-test-board-persist-blocked-%06x" (Random.bits ()))
  in
  Unix.putenv "MASC_BASE_PATH" base;
  let masc_dir = Filename.dirname (Board.persist_path ()) in
  Fs_compat.mkdir_p (Filename.dirname masc_dir);
  Fs_compat.save_file masc_dir "not a directory";
  base

let seed_post_without_kind () =
  let now = Time_compat.now () in
  let post_id = Printf.sprintf "missing-kind-%06x" (Random.bits ()) in
  let path = Board.persist_path () in
  let dir = Filename.dirname path in
  Fs_compat.mkdir_p dir;
  let json =
    `Assoc
      [
        ("id", `String post_id);
        ("author", `String "dm-keeper");
        ("title", `String "Missing kind");
        ("body", `String "keeper");
        ("content", `String "keeper");
        ("visibility", `String "internal");
        ("created_at", `Float now);
        ("updated_at", `Float now);
        ("expires_at", `Float 0.0);
        ("votes_up", `Int 0);
        ("votes_down", `Int 0);
        ("score", `Int 0);
        ("reply_count", `Int 0);
        ("pinned", `Bool false);
        ("meta", `Assoc [ ("source", `String "masc_board_post") ]);
      ]
  in
  Fs_compat.append_file path (Yojson.Safe.to_string json ^ "\n");
  Board.reset_global_for_test ();
  Board_dispatch.reset_for_test ();
  Board_dispatch.init_jsonl ();
  post_id

let keeper_meta name =
  match
    Masc_test_deps.meta_of_json_fixture
      (`Assoc
        [ "name", `String name
        ; "trace_id", `String ("test-trace-" ^ name)
        ])
  with
  | Ok meta -> meta
  | Error error -> Alcotest.failf "keeper meta fixture invalid: %s" error

(** {1 Backend Selection} *)

let test_default_backend () =
  Alcotest.(check string) "default is jsonl"
    "jsonl" (Board_dispatch.backend_name ())

let test_backend_returns_jsonl () =
  match Board_dispatch.backend () with
  | Board_dispatch.Jsonl _ -> ()

(** {1 Post CRUD via Dispatch} *)

let test_create_and_get_post () =
  match
    Board_dispatch.create_post ~author:"test-agent" ~content:"dispatch test post"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      Alcotest.(check string) "author" "test-agent" (Board.Agent_id.to_string post.author);
      match Board_dispatch.get_post ~post_id:pid with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok fetched ->
          Alcotest.(check string) "content matches"
            "dispatch test post" fetched.body

let test_update_post_by_owner () =
  match
    Board_dispatch.create_post ~author:"editor-agent"
      ~content:"original body before edit" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"editor-agent"
          ~content:"edited body after change" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok updated -> (
          Alcotest.(check string) "content updated" "edited body after change"
            updated.body;
          Alcotest.(check string) "body updated" "edited body after change"
            updated.body;
          Alcotest.(check bool) "updated_at not regressed" true
            (updated.updated_at >= post.updated_at);
          (* edit persists: a fresh get returns the edited content *)
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok fetched ->
              Alcotest.(check string) "edit persisted via get"
                "edited body after change" fetched.body))

let test_update_post_rejects_non_owner () =
  match
    Board_dispatch.create_post ~author:"owner-agent"
      ~content:"body owned by owner-agent" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"intruder-agent"
          ~content:"hijacked content" ()
      with
      | Ok _ -> Alcotest.fail "non-owner edit must be rejected"
      | Error (Board.Unauthorized _) -> (
          (* the original content must survive a rejected edit *)
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok fetched ->
              Alcotest.(check string) "original preserved on rejected edit"
                "body owned by owner-agent" fetched.body)
      | Error e ->
          Alcotest.fail ("expected Unauthorized, got " ^ Board.show_board_error e))

let test_update_post_transfers_author_by_owner () =
  match
    Board_dispatch.create_post ~author:"transfer-owner"
      ~content:"transfer original body" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"transfer-owner"
          ~content:"transfer updated body" ~new_author:"transfer-next" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok updated -> (
          Alcotest.(check string) "author transferred" "transfer-next"
            (Board.Agent_id.to_string updated.author);
          Alcotest.(check string) "content updated during transfer"
            "transfer updated body" updated.body;
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok fetched ->
              Alcotest.(check string) "transferred author persisted"
                "transfer-next"
                (Board.Agent_id.to_string fetched.author)))

let test_update_post_rejects_non_owner_transfer () =
  match
    Board_dispatch.create_post ~author:"transfer-owner"
      ~content:"non-owner transfer original" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"transfer-intruder"
          ~content:"hijacked transfer body" ~new_author:"transfer-intruder" ()
      with
      | Ok _ -> Alcotest.fail "non-owner transfer must be rejected"
      | Error (Board.Unauthorized _) -> (
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok fetched ->
              Alcotest.(check string) "original author preserved"
                "transfer-owner"
                (Board.Agent_id.to_string fetched.author);
              Alcotest.(check string) "original content preserved"
                "non-owner transfer original" fetched.body)
      | Error e ->
          Alcotest.fail ("expected Unauthorized, got " ^ Board.show_board_error e))

let test_update_post_rejects_invalid_new_author () =
  match
    Board_dispatch.create_post ~author:"transfer-owner"
      ~content:"invalid transfer original" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"transfer-owner"
          ~content:"invalid transfer body" ~new_author:"not a valid author" ()
      with
      | Ok _ -> Alcotest.fail "invalid new_author must be rejected"
      | Error (Board.Validation_error _) -> (
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok fetched ->
              Alcotest.(check string) "author preserved after invalid transfer"
                "transfer-owner"
                (Board.Agent_id.to_string fetched.author);
              Alcotest.(check string) "content preserved after invalid transfer"
                "invalid transfer original" fetched.body)
      | Error e ->
          Alcotest.fail
            ("expected Validation_error, got " ^ Board.show_board_error e))

let test_update_post_missing_id () =
  match
    Board_dispatch.update_post ~post_id:"missing-post-id-zzz" ~editor:"any-agent"
      ~content:"replacement" ()
  with
  | Ok _ -> Alcotest.fail "edit of missing post must fail"
  | Error (Board.Post_not_found _) -> ()
  | Error e ->
      Alcotest.fail ("expected Post_not_found, got " ^ Board.show_board_error e)

let test_update_post_rejects_empty_content () =
  match
    Board_dispatch.create_post ~author:"empty-edit-agent"
      ~content:"non-empty original" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"empty-edit-agent"
          ~content:"   " ()
      with
      | Ok _ -> Alcotest.fail "empty content edit must fail"
      | Error (Board.Validation_error _) -> ()
      | Error e ->
          Alcotest.fail
            ("expected Validation_error, got " ^ Board.show_board_error e))

(* Exercises the explicit [?title]/[?body] params (the default-only path is
   covered by [test_update_post_by_owner]) and asserts the fields documented as
   immutable on edit — [post_kind] and [visibility] — are preserved. *)
let test_update_post_with_explicit_title_and_body () =
  match
    Board_dispatch.create_post ~author:"titler-agent"
      ~content:"original" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"titler-agent"
          ~content:"content fallback" ~title:"Explicit Title"
          ~body:"Explicit body text" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok updated ->
          Alcotest.(check string) "title from ?title" "Explicit Title"
            updated.title;
          Alcotest.(check string) "body from ?body" "Explicit body text"
            updated.body;
          Alcotest.(check string) "content tracks body" "Explicit body text"
            updated.body;
          Alcotest.(check bool) "post_kind preserved" true
            (updated.post_kind = post.post_kind);
          Alcotest.(check bool) "visibility preserved" true
            (updated.visibility = post.visibility))

let meta_has_source (meta : Yojson.Safe.t option) : bool =
  match meta with
  | Some (`Assoc fields) -> List.mem_assoc "source" fields
  | _ -> false

(* stdlib-only substring containment; avoids a [re] dep for this test exe. *)
let check_io_error ~where = function
  | Error (Board.Io_error msg) ->
      Alcotest.(check bool)
        ("error includes " ^ where)
        true
        (String_util.string_contains_substring ~needle:where msg)
  | Error e -> Alcotest.fail ("expected Io_error, got " ^ Board.show_board_error e)
  | Ok _ -> Alcotest.fail "expected Io_error"

let test_update_post_preserves_meta_and_body () =
  match
    Board_dispatch.create_post ~author:"stateful-agent"
      ~content:"plain original body" ~post_kind:Board.Human_post
      ~meta_json:(`Assoc [ "source", `String "operator" ]) ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> (
      Alcotest.(check bool) "original metadata present" true
        (meta_has_source post.meta_json);
      let pid = Board.Post_id.to_string post.id in
      match
        Board_dispatch.update_post ~post_id:pid ~editor:"stateful-agent"
          ~content:"visible text with ordinary brackets [progress: halfway]" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok updated ->
          Alcotest.(check bool) "metadata preserved" true
            (meta_has_source updated.meta_json);
          Alcotest.(check string) "body preserved verbatim"
            "visible text with ordinary brackets [progress: halfway]"
            updated.body)

let test_keeper_signal_hook_failure_does_not_abort_create_post () =
  Board_dispatch.set_board_signal_hook (fun _ ->
      failwith "keeper signal hook failed");
  match
    Board_dispatch.create_post ~author:"test-agent"
      ~content:"post must survive keeper wake failure"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      Alcotest.(check string) "content persisted despite hook failure"
        "post must survive keeper wake failure" post.body

let test_keeper_signal_hook_cancellation_propagates () =
  Board_dispatch.set_board_signal_hook (fun _ ->
      raise (Eio.Cancel.Cancelled (Failure "synthetic-cancel")));
  let raised = ref false in
  (try
     ignore
       (Board_dispatch.create_post ~author:"test-agent"
          ~content:"cancel should propagate through keeper wake hook"
          ~post_kind:Board.Human_post ());
   with Eio.Cancel.Cancelled _ -> raised := true);
  Alcotest.(check bool) "cancellation propagated" true !raised

(* [create_post] has no content dedup: identical bodies from one keeper turn
   become two posts and fan out twice. The idempotent entry point is
   [create_post_once_by_fusion_run_id], which keys on a run id rather than on
   content.

   Pinned rather than left unasserted because the read side has no defence: a
   keeper that posts the same body twice reaches every subscriber twice. If
   dedup returns, this fails and the author has to say so. *)
let test_identical_content_creates_two_posts () =
  let keeper_signals = ref 0 in
  let sse_post_created = ref 0 in
  Board_dispatch.set_board_signal_hook (fun _ -> incr keeper_signals);
  Board_dispatch.set_board_sse_hook (function
    | Board_dispatch.Post_created _ -> incr sse_post_created
    | _ -> ());
  let create () =
    Board_dispatch.create_post ~author:"dedup-agent"
      ~content:"same post body from one keeper turn"
      ~post_kind:Board.Automation_post ~hearth:"keepers" ~thread_id:"turn-15650" ()
  in
  let first =
    match create () with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let second =
    match create () with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  Alcotest.(check bool)
    "identical content yields a distinct post"
    false
    (String.equal
       (Board.Post_id.to_string first.id)
       (Board.Post_id.to_string second.id));
  Alcotest.(check int) "keeper signal emitted per post" 2 !keeper_signals;
  Alcotest.(check int) "SSE post_created emitted per post" 2 !sse_post_created

let test_create_post_persistence_failure_returns_error_without_fanout () =
  let keeper_signals = ref 0 in
  let sse_post_created = ref 0 in
  Board_dispatch.set_board_signal_hook (fun _ -> incr keeper_signals);
  Board_dispatch.set_board_sse_hook (function
    | Board_dispatch.Post_created _ -> incr sse_post_created
    | _ -> ());
  ignore (block_board_masc_dir_with_file ());
  let before_errors = Board.persist_error_count () in
  check_io_error
    ~where:"append_post"
    (Board_dispatch.create_post
       ~author:"persist-fail-agent"
       ~content:"this post must not survive a failed append"
       ~post_kind:Board.Human_post
       ());
  Alcotest.(check bool)
    "persist error counter incremented"
    true
    (Board.persist_error_count () > before_errors);
  Alcotest.(check int) "keeper signal not emitted" 0 !keeper_signals;
  Alcotest.(check int) "SSE post_created not emitted" 0 !sse_post_created;
  Alcotest.(check int)
    "failed create rolled back in-memory post"
    0
    (List.length (Board_dispatch.list_posts ~limit:10 ()))

let test_structured_post_roundtrip () =
  let meta = `Assoc [("source", `String "keeper_autonomy")] in
  match Board_dispatch.create_post ~author:"alpha"
          ~title:"Explicit title"
          ~content:"Visible line\n\nSupporting detail"
          ~post_kind:Board.Automation_post
          ~meta_json:meta
          () with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      Alcotest.(check string) "title stored" "Explicit title" post.title;
      Alcotest.(check string) "body stored" "Visible line\n\nSupporting detail" post.body;
      Alcotest.(check bool) "metadata stored" true (meta_has_source post.meta_json);
      Board.reset_global_for_test ();
      Board_dispatch.reset_for_test ();
      Board_dispatch.init_jsonl ();
      match Board_dispatch.get_post ~post_id:pid with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok fetched ->
          Alcotest.(check string) "roundtrip title" "Explicit title" fetched.title;
          Alcotest.(check string) "roundtrip content alias"
            "Visible line\n\nSupporting detail" fetched.body;
          Alcotest.(check string) "roundtrip body"
            "Visible line\n\nSupporting detail" fetched.body;
          Alcotest.(check string) "roundtrip kind" "automation"
            (Board.post_kind_to_string fetched.post_kind)

let test_board_sse_post_created_includes_post_kind () =
  let seen = ref None in
  Board_dispatch.set_board_sse_hook (fun event -> seen := Some event);
  (match
     Board_dispatch.create_post ~author:"sse-agent"
       ~content:"sse post kind payload"
       ~post_kind:Board.Automation_post ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  match !seen with
  | Some (Board_dispatch.Post_created { post_kind; _ }) ->
      Alcotest.(check string) "sse post_kind" "automation"
        (Board.post_kind_to_string post_kind)
  | Some _ -> Alcotest.fail "expected post_created SSE event"
  | None -> Alcotest.fail "expected board SSE event"

let test_list_posts () =
  ignore (Board_dispatch.create_post ~author:"lister" ~content:"list test 1"
            ~post_kind:Board.Human_post ());
  ignore (Board_dispatch.create_post ~author:"lister" ~content:"list test 2"
            ~post_kind:Board.Human_post ());
  let posts = Board_dispatch.list_posts ~limit:10 () in
  Alcotest.(check bool) "at least 2 posts" true (List.length posts >= 2)

let test_list_posts_negative_limit_returns_empty () =
  ignore (Board_dispatch.create_post ~author:"negative-limit"
            ~content:"negative limit probe" ~post_kind:Board.Human_post ());
  let posts = Board_dispatch.list_posts ~limit:(-1) () in
  Alcotest.(check int) "negative limit closes to empty" 0 (List.length posts)

let test_list_posts_with_sort () =
  let posts_hot = Board_dispatch.list_posts ~sort_by:Board_dispatch.Hot ~limit:5 () in
  let posts_recent = Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent ~limit:5 () in
  let posts_trending = Board_dispatch.list_posts ~sort_by:Board_dispatch.Trending ~limit:5 () in
  let posts_updated = Board_dispatch.list_posts ~sort_by:Board_dispatch.Updated ~limit:5 () in
  let posts_discussed = Board_dispatch.list_posts ~sort_by:Board_dispatch.Discussed ~limit:5 () in
  let counts = List.map List.length [posts_hot; posts_recent; posts_trending; posts_updated; posts_discussed] in
  let all_same = List.for_all (fun c -> c = List.hd counts) counts in
  Alcotest.(check bool) "all sort orders return same count" true all_same

(* A hand-written meta object cannot stay valid: the current schema requires
   some fifty fields and rejects anything outside itself, so every field the
   schema gains or moves breaks this fixture. [meta_of_json_fixture] fills the
   rest from the canonical defaults, which is also how
   [test_keeper_waiting_inventory] recovered from the same two drifts —
   [agent_name] spelled by hand, and [sandbox_profile] / [network_mode], which
   are keeper TOML fields rather than meta JSON fields. *)
let board_observation_meta name =
  match Masc_test_deps.meta_of_json_fixture (`Assoc [ "name", `String name ]) with
  | Ok meta -> meta
  | Error message -> Alcotest.failf "board observation meta failed: %s" message

let test_first_board_observation_starts_at_current_head () =
  let base_path = Sys.getenv "MASC_BASE_PATH" in
  let keeper_name = "cursor-bootstrap" in
  let meta = board_observation_meta keeper_name in
  ignore (Keeper_registry.For_testing.register ~base_path keeper_name meta);
  Fun.protect
    ~finally:(fun () -> Keeper_registry.For_testing.unregister ~base_path keeper_name)
    (fun () ->
       let old_post =
         match
           Board_dispatch.create_post
             ~author:"external-author"
             ~content:("@" ^ keeper_name ^ " historical post must not replay")
             ~post_kind:Board.Human_post
             ()
         with
         | Ok post -> post
         | Error error -> Alcotest.fail (Board.show_board_error error)
       in
       let events, new_count, mention_count =
         Keeper_world_observation.collect_board_events ~base_path ~meta
       in
       Alcotest.(check int)
         "historical events are not replayed"
         0
         (List.length events);
       Alcotest.(check int) "historical post is not counted as new" 0 new_count;
       Alcotest.(check int) "historical mention is not counted" 0 mention_count;
       let cursor_ts, cursor_post_id =
         Keeper_registry.get_board_cursor ~base_path keeper_name
       in
       Alcotest.(check (float 0.0))
         "cursor starts at exact current Board head"
         old_post.updated_at
         cursor_ts;
       Alcotest.(check (option string))
         "cursor records current head post id"
         (Some (Board.Post_id.to_string old_post.id))
         cursor_post_id;
       Unix.sleepf 0.01;
       let new_post =
         match
           Board_dispatch.create_post
             ~author:"external-author"
             ~content:("@" ^ keeper_name ^ " new post must be observed")
             ~post_kind:Board.Human_post
             ()
         with
         | Ok post -> post
         | Error error -> Alcotest.fail (Board.show_board_error error)
       in
       let events, new_count, mention_count =
         Keeper_world_observation.collect_board_events ~base_path ~meta
       in
       Alcotest.(check int) "new event is observed once" 1 (List.length events);
       Alcotest.(check int) "new post count" 1 new_count;
       Alcotest.(check int) "new mention count" 1 mention_count;
       match events with
       | [ event ] ->
         Alcotest.(check string)
           "new event id"
           (Board.Post_id.to_string new_post.id)
           event.Keeper_world_observation.post_id
       | _ -> Alcotest.fail "expected exactly one new Board event")

let test_dashboard_projection_does_not_produce_attention_candidate () =
  let base_path = Sys.getenv "MASC_BASE_PATH" in
  let keeper_name = "projection-read-only" in
  let meta = board_observation_meta keeper_name in
  let entry = Keeper_registry.For_testing.register ~base_path keeper_name meta in
  Fun.protect
    ~finally:(fun () -> Keeper_registry.For_testing.unregister ~base_path keeper_name)
    (fun () ->
       ignore
         (Keeper_world_observation.collect_board_events ~base_path ~meta
           : Keeper_world_observation.pending_board_event list * int * int);
       Unix.sleepf 0.01;
       (match
          Board_dispatch.create_post
            ~author:"external-author"
            ~content:"ambient post requiring the structured attention judge"
            ~post_kind:Board.Human_post
            ()
        with
        | Ok _ -> ()
        | Error error -> Alcotest.fail (Board.show_board_error error));
       ignore
         (Keeper_world_observation.collect_board_events_without_advancing_cursor
            ~base_path
            ~meta
           : Keeper_world_observation.pending_board_event list * int * int);
       (match
          Keeper_board_attention_candidate.load_candidates
            ~base_path
            ~keeper_name
        with
        | Ok [] -> ()
        | Ok candidates ->
          Alcotest.failf
            "read-only projection persisted %d attention candidates"
            (List.length candidates)
        | Error detail ->
          Alcotest.failf "candidate projection read failed: %s" detail);
       Alcotest.(check bool)
         "read-only projection did not wake the Keeper"
         false
         (Atomic.get entry.fiber_wakeup);
       ignore
         (Keeper_world_observation.collect_board_events ~base_path ~meta
           : Keeper_world_observation.pending_board_event list * int * int);
       (match
          Keeper_board_attention_candidate.load_candidates
            ~base_path
            ~keeper_name
        with
        | Ok [ { status = Keeper_board_attention_candidate.Pending _; _ } ] -> ()
        | Ok candidates ->
          Alcotest.failf
            "live owner collection persisted %d candidates instead of one Pending"
            (List.length candidates)
        | Error detail ->
          Alcotest.failf "owner candidate read failed: %s" detail);
       (* The live collector records the candidate and asks
          [Keeper_board_attention_worker_wake] for a judgment. Neither it nor
          [Wake.request] touches [fiber_wakeup]: the keeper wakes once the
          worker has judged, which [test_keeper_board_attention_worker] covers.
          Pinned as [false] so a synchronous wake here — which would put the
          keeper on a candidate the judge has not seen — fails. *)
       Alcotest.(check bool)
         "live owner collection defers the wake to the judgment worker"
         false
         (Atomic.get entry.fiber_wakeup))
;;

(* [vote] returns the post's score after the vote, not the amount this vote
   changed it. The two are equal for the first voter, which is why the field
   could sit under the name [delta] and read correctly everywhere it was tried
   (#27675). A second voter separates them: the total is 2, a change is 1. *)
let test_vote_returns_total_score_not_change () =
  let post =
    match
      Board_dispatch.create_post ~author:"score-author"
        ~content:"two voters separate a total from a change" ~post_kind:Board.Human_post ()
    with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let post_id = Board.Post_id.to_string post.id in
  let vote_exn ~voter =
    match Board_dispatch.vote ~voter ~post_id ~direction:Board.Up with
    | Ok score -> score
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let first = vote_exn ~voter:"score-voter-a" in
  let second = vote_exn ~voter:"score-voter-b" in
  Alcotest.(check int) "first vote: total and change agree" 1 first;
  Alcotest.(check int) "second vote returns the total, not the change" 2 second
;;

let test_recent_sort_bypasses_hot_cutoff () =
  let create_post_exn ~author ~content =
    match
      Board_dispatch.create_post ~author ~content
        ~post_kind:Board.Human_post ()
    with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let vote_up_exn ~post_id ~voter =
    match Board_dispatch.vote ~voter ~post_id ~direction:Board.Up with
    | Ok _ -> ()
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  for i = 1 to 101 do
    let hot_post =
      create_post_exn ~author:(Printf.sprintf "hot-author-%03d" i)
        ~content:(Printf.sprintf "hot post %03d" i)
    in
    vote_up_exn ~post_id:(Board.Post_id.to_string hot_post.id)
      ~voter:(Printf.sprintf "hot-voter-%03d" i)
  done;
  let cold_post =
    create_post_exn ~author:"recent-cold-author"
      ~content:"latest cold post should still win recent sort"
  in
  let cold_post_id = Board.Post_id.to_string cold_post.id in
  let recent_posts =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent ~limit:1 ()
  in
  let hot_posts =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Hot ~limit:1 ()
  in
  let recent_post_id =
    match recent_posts with
    | post :: _ -> Board.Post_id.to_string post.id
    | [] -> Alcotest.fail "expected recent posts"
  in
  let hot_post_id =
    match hot_posts with
    | post :: _ -> Board.Post_id.to_string post.id
    | [] -> Alcotest.fail "expected hot posts"
  in
  Alcotest.(check string) "recent returns latest post beyond hot top 100"
    cold_post_id recent_post_id;
  Alcotest.(check bool) "hot ranking still excludes cold post" false
    (String.equal hot_post_id cold_post_id)

let test_list_posts_with_filters () =
  let keeper_meta = `Assoc [ ("source", `String "masc_board_post") ] in
  let scoped_authors = [ "filter-human"; "filter-harness-bot"; "filter-keeper" ] in
  let is_scoped_author (p : Board.post) =
    List.mem (Board.Agent_id.to_string p.author) scoped_authors
  in
  ignore (Board_dispatch.create_post ~author:"filter-human" ~content:"human-filter-test"
            ~post_kind:Board.Human_post ());
  ignore (Board_dispatch.create_post ~author:"filter-harness-bot"
            ~content:"automation" ~visibility:Board.Internal ~ttl_hours:1
            ~hearth:"dashboard-harness" ~post_kind:Board.Automation_post ());
  ignore (Board_dispatch.create_post ~author:"filter-keeper" ~content:"keeper"
            ~post_kind:Board.Automation_post ~meta_json:keeper_meta ());
  let all_posts =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent ~limit:50 ()
    |> List.filter is_scoped_author
  in
  let no_system =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent ~exclude_system:true
      ~limit:50 ()
    |> List.filter is_scoped_author
  in
  let no_automation =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent
      ~exclude_automation:true ~limit:50 ()
    |> List.filter is_scoped_author
  in
  let human_only =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent ~exclude_system:true
      ~exclude_automation:true ~limit:50 ()
    |> List.filter is_scoped_author
  in
  Alcotest.(check int) "all posts" 3 (List.length all_posts);
  Alcotest.(check int) "exclude system" 3 (List.length no_system);
  Alcotest.(check int) "exclude automation" 1 (List.length no_automation);
  Alcotest.(check int) "exclude both" 1 (List.length human_only);
  Alcotest.(check string) "human remains" "filter-human"
    (human_only |> List.hd |> fun (p : Board.post) -> Board.Agent_id.to_string p.author)

let test_list_posts_matches_comment_author () =
  let matching_post =
    match
      Board_dispatch.create_post ~author:"post-owner-a"
        ~content:"comment author should surface this post"
        ~post_kind:Board.Human_post ()
    with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let other_post =
    match
      Board_dispatch.create_post ~author:"post-owner-b"
        ~content:"different comment author"
        ~post_kind:Board.Human_post ()
    with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  let matching_post_id = Board.Post_id.to_string matching_post.id in
  let other_post_id = Board.Post_id.to_string other_post.id in
  (match
     Board_dispatch.add_comment ~post_id:matching_post_id
       ~author:"comment-match-agent" ~content:"I touched this thread" ()
   with
   | Ok _ -> ()
   | Error e -> Alcotest.fail (Board.show_board_error e));
  (match
     Board_dispatch.add_comment ~post_id:other_post_id
       ~author:"comment-other-agent" ~content:"Different author" ()
   with
   | Ok _ -> ()
   | Error e -> Alcotest.fail (Board.show_board_error e));
  let filtered =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent
      ~author_filter:"MATCH-AGENT" ~limit:20 ()
  in
  let ids =
    List.map (fun (post : Board.post) -> Board.Post_id.to_string post.id) filtered
  in
  Alcotest.(check bool) "matching comment author includes post" true
    (List.mem matching_post_id ids);
  Alcotest.(check bool) "non matching comment author excluded" false
    (List.mem other_post_id ids)

let test_author_filter_treats_wildcards_literally () =
  ignore
    (Board_dispatch.create_post ~author:"wildcard-alpha"
       ~content:"literal wildcard filter" ~post_kind:Board.Human_post ());
  let filtered =
    Board_dispatch.list_posts ~sort_by:Board_dispatch.Recent
      ~author_filter:"%" ~limit:20 ()
  in
  Alcotest.(check int) "percent does not match all authors" 0
    (List.length filtered)

let test_recent_author_match_applies_before_limit () =
  let own =
    match
      Board_dispatch.create_post ~author:"exact-owner"
        ~content:"owned before unrelated traffic"
        ~post_kind:Board.Human_post ()
    with
    | Ok post -> post
    | Error e -> Alcotest.fail (Board.show_board_error e)
  in
  for index = 1 to 3 do
    ignore
      (Board_dispatch.create_post
         ~author:(Printf.sprintf "unrelated-%d" index)
         ~content:"newer unrelated traffic"
         ~post_kind:Board.Human_post
         ())
  done;
  let posts =
    Board_dispatch.list_recent_posts_matching_author
      ~author_matches:(fun author ->
        String.equal (Board.Agent_id.to_string author) "exact-owner")
      ~limit:1
      ()
  in
  match posts with
  | [ post ] ->
    Alcotest.(check string)
      "newer unrelated posts do not consume the result limit"
      (Board.Post_id.to_string own.id)
      (Board.Post_id.to_string post.id)
  | _ -> Alcotest.fail "expected exactly one matching author post"

let test_post_without_kind_is_rejected () =
  let post_id = seed_post_without_kind () in
  match Board_dispatch.get_post ~post_id with
  | Error (Board.Post_not_found _) -> ()
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok _ -> Alcotest.fail "post without post_kind must be rejected"

(** {1 Comment Operations} *)

let test_add_and_get_comments () =
  match
    Board_dispatch.create_post ~author:"commenter" ~content:"post for comments"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      (match Board_dispatch.add_comment ~post_id:pid ~author:"responder" ~content:"nice post" () with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok _ -> ());
      match Board_dispatch.get_comments ~post_id:pid with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok comments ->
          Alcotest.(check bool) "has comment" true (List.length comments >= 1)

let test_comment_persists_post_reply_count () =
  let post =
    match
      Board_dispatch.create_post ~author:"automation-author"
        ~content:"automation post for reply_count persistence"
        ~post_kind:Board.Automation_post ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  (match
     Board_dispatch.add_comment ~post_id ~author:"automation-commenter"
       ~content:"automation/direct comment reply" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  (* Write-ahead (#28952): the comment append is the durable WAL row;
     the posts snapshot picks up the bumped reply_count on the next
     flush, exactly like vote counts — not inline with the comment. *)
  (match Board_dispatch.backend () with
   | Board_dispatch.Jsonl store -> Board.flush_dirty store);
  let persisted_reply_count =
    Board.persist_path ()
    |> Fs_compat.load_jsonl
    |> List.find_map (fun json ->
      match Safe_ops.json_string_opt "id" json with
      | Some id when String.equal id post_id ->
        Some (Safe_ops.json_int ~default:(-1) "reply_count" json)
      | _ -> None)
  in
  Alcotest.(check (option int))
    "post snapshot reply_count updated by the flush after a comment"
    (Some 1)
    persisted_reply_count;
  Board.reset_global_for_test ();
  Board_dispatch.reset_for_test ();
  Board_dispatch.init_jsonl ();
  match Board_dispatch.get_post ~post_id with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok fetched ->
    Alcotest.(check int) "reply_count survives restart" 1 fetched.reply_count

let test_add_comment_persistence_failure_rolls_back_without_fanout () =
  let post =
    match
      Board_dispatch.create_post
        ~author:"comment-persist-fail-author"
        ~content:"post before comment append failure"
        ~post_kind:Board.Human_post
        ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  let keeper_signals = ref 0 in
  let sse_comment_added = ref 0 in
  Board_dispatch.set_board_signal_hook (fun _ -> incr keeper_signals);
  Board_dispatch.set_board_sse_hook (function
    | Board_dispatch.Comment_added _ -> incr sse_comment_added
    | _ -> ());
  ignore (block_board_masc_dir_with_file ());
  let before_errors = Board.persist_error_count () in
  check_io_error
    ~where:"append_comment"
    (Board_dispatch.add_comment
       ~post_id
       ~author:"comment-persist-fail-responder"
       ~content:"this comment must not survive a failed append"
       ());
  Alcotest.(check bool)
    "persist error counter incremented"
    true
    (Board.persist_error_count () > before_errors);
  Alcotest.(check int) "keeper signal not emitted" 0 !keeper_signals;
  Alcotest.(check int) "SSE comment_added not emitted" 0 !sse_comment_added;
  (match Board_dispatch.get_post ~post_id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok fetched ->
       Alcotest.(check int) "reply_count rolled back" 0 fetched.reply_count);
  match Board_dispatch.get_comments ~post_id with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok comments ->
      Alcotest.(check int)
        "failed comment rolled back in-memory comment"
        0
        (List.length comments)

let test_get_post_and_comments_atomic () =
  match
    Board_dispatch.create_post ~author:"atomic-author"
      ~content:"atomic read body" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      (match
         Board_dispatch.add_comment ~post_id:pid ~author:"first"
           ~content:"first comment" ()
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok _ -> ());
      (match
         Board_dispatch.add_comment ~post_id:pid ~author:"second"
           ~content:"second comment" ()
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok _ -> ());
      match Board_dispatch.get_post_and_comments ~post_id:pid () with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok (fetched, comments) ->
          Alcotest.(check string) "post content matches"
            "atomic read body" fetched.body;
          Alcotest.(check int) "comment count" 2 (List.length comments)

let test_get_post_and_comments_pagination_clamps () =
  match
    Board_dispatch.create_post ~author:"page-author"
      ~content:"paged read body" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      for i = 1 to 3 do
        match
          Board_dispatch.add_comment ~post_id:pid
            ~author:(Printf.sprintf "commenter-%d" i)
            ~content:(Printf.sprintf "comment-%d" i)
            ()
        with
        | Error e -> Alcotest.fail (Board.show_board_error e)
        | Ok _ -> ()
      done;
      (match Board_dispatch.get_post_and_comments ~post_id:pid () with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok (_, comments) ->
           Alcotest.(check int) "omitted pagination returns all" 3
             (List.length comments));
      (match
         Board_dispatch.get_post_and_comments ~post_id:pid ~comment_offset:(-5)
           ~comment_limit:(-1) ()
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok (_, comments) ->
           Alcotest.(check int) "negative pagination clamps to one" 1
             (List.length comments);
           let first =
             match comments with
             | (comment : Board.comment) :: _ -> comment.content
             | [] -> Alcotest.fail "expected one clamped comment"
           in
           Alcotest.(check string) "negative pagination starts at first" "comment-1"
             first);
      match
        Board_dispatch.get_post_and_comments ~post_id:pid ~comment_offset:2
          ~comment_limit:999 ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok (_, comments) ->
          Alcotest.(check int) "over max limit clamps after offset" 1
            (List.length comments)

let test_get_post_and_comments_missing_post () =
  match Board_dispatch.get_post_and_comments ~post_id:"never-existed" () with
  | Ok _ -> Alcotest.fail "expected Post_not_found"
  | Error (Board.Post_not_found _) -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "expected Post_not_found, got %s"
           (Board.show_board_error e))

(** {1 Vote Operations} *)

let test_vote_post () =
  match
    Board_dispatch.create_post ~author:"voter-test" ~content:"vote me"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      match Board_dispatch.vote ~voter:"judge" ~post_id:pid ~direction:Board.Up with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok score ->
          Alcotest.(check int) "score after upvote" 1 score

let test_vote_dedup () =
  match
    Board_dispatch.create_post ~author:"dedup-test" ~content:"dedup vote"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      ignore (Board_dispatch.vote ~voter:"same-voter" ~post_id:pid ~direction:Board.Up);
      match Board_dispatch.vote ~voter:"same-voter" ~post_id:pid ~direction:Board.Up with
      | Ok _ -> Alcotest.fail "Expected Already_voted error"
      | Error (Board.Already_voted _) -> ()
      | Error e -> Alcotest.fail (Board.show_board_error e)

let test_vote_flip () =
  match
    Board_dispatch.create_post ~author:"flip-test" ~content:"flip vote"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      ignore (Board_dispatch.vote ~voter:"flipper" ~post_id:pid ~direction:Board.Up);
      match Board_dispatch.vote ~voter:"flipper" ~post_id:pid ~direction:Board.Down with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok score ->
          Alcotest.(check int) "score after flip" (-1) score

let test_current_vote_lookup () =
  let vote_label = Option.map Board.vote_direction_to_string in
  match
    Board_dispatch.create_post ~author:"state-test" ~content:"state vote"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      (match Board_dispatch.current_vote_for_post ~voter:"reader" ~post_id:pid with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok vote ->
           Alcotest.(check (option string)) "post vote starts empty" None
             (vote_label vote));
      ignore (Board_dispatch.vote ~voter:"reader" ~post_id:pid ~direction:Board.Up);
      (match Board_dispatch.current_vote_for_post ~voter:"reader" ~post_id:pid with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok vote ->
           Alcotest.(check (option string)) "post vote state" (Some "up")
             (vote_label vote));
      (match
         Board_dispatch.add_comment ~post_id:pid ~author:"commenter"
           ~content:"vote this comment" ()
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok comment ->
           let cid = Board.Comment_id.to_string comment.id in
           (match
              Board_dispatch.current_vote_for_comment ~voter:"reader"
                ~comment_id:cid
            with
            | Error e -> Alcotest.fail (Board.show_board_error e)
            | Ok vote ->
                Alcotest.(check (option string)) "comment vote starts empty" None
                  (vote_label vote));
           ignore
             (Board_dispatch.vote_comment ~voter:"reader" ~comment_id:cid
                ~direction:Board.Down);
           match
             Board_dispatch.current_vote_for_comment ~voter:"reader"
               ~comment_id:cid
           with
           | Error e -> Alcotest.fail (Board.show_board_error e)
           | Ok vote ->
               Alcotest.(check (option string)) "comment vote state"
                 (Some "down") (vote_label vote))

let test_vote_persisted_by_flusher_actor () =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    Fs_compat.set_fs (Eio.Stdenv.fs env);
    Eio_context.with_test_env
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~sw
      (fun () ->
        ignore (fresh_test_base_path ());
        Board.reset_global_for_test ();
        Board_dispatch.reset_for_test ();
        Board_dispatch.init_jsonl ();
        let post_id =
          match
            Board_dispatch.create_post ~author:"persist-test" ~content:"persist my vote"
              ~post_kind:Board.Human_post ()
          with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok post -> Board.Post_id.to_string post.id
        in
        (match Board_dispatch.vote ~voter:"judge" ~post_id ~direction:Board.Up with
         | Error e -> Alcotest.fail (Board.show_board_error e)
         | Ok _ -> ());
        let store =
          match Board_dispatch.backend () with
          | Board_dispatch.Jsonl store -> store
        in
        store.last_flush <- 0.0;
        (match Board_dispatch.get_post ~post_id with
         | Ok _ -> ()
         | Error e -> Alcotest.fail (Board.show_board_error e));
        Eio.Time.sleep clock 0.1;
        Board.reset_global_for_test ();
        Board_dispatch.reset_for_test ();
        Board_dispatch.init_jsonl ();
        (match Board_dispatch.get_post ~post_id with
         | Error e -> Alcotest.fail (Board.show_board_error e)
         | Ok post ->
             Alcotest.(check int) "vote persisted after restart" 1 post.votes_up);
        Eio.Switch.fail sw Exit)
  with Exit -> ()

let test_flusher_start_retries_forced_cas_conflicts () =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    Fs_compat.set_fs (Eio.Stdenv.fs env);
    Eio_context.with_test_env
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~sw
      (fun () ->
        ignore (fresh_test_base_path ());
        Board.reset_global_for_test ();
        Board_dispatch.reset_for_test ();
        Board_dispatch.force_flusher_start_cas_conflicts_for_test 2;
        Board_dispatch.init_jsonl ();
        Alcotest.(check bool)
          "flusher starts after forced CAS contention" true
          (Board_dispatch.flusher_started_for_test ());
        Eio.Switch.fail sw Exit)
  with Exit -> ()

let test_flusher_start_backoff_delay_doubles_and_caps () =
  Alcotest.(check (float 0.0001))
    "attempt 0 delay" 0.001
    (Board_dispatch.flusher_start_backoff_delay_for_test ~attempt:0);
  Alcotest.(check (float 0.0001))
    "attempt 1 delay doubles" 0.002
    (Board_dispatch.flusher_start_backoff_delay_for_test ~attempt:1);
  Alcotest.(check (float 0.0001))
    "large attempt caps" 0.02
    (Board_dispatch.flusher_start_backoff_delay_for_test ~attempt:10)

(** {1 Reaction Operations} *)

let test_reaction_toggle_and_summary () =
  match
    Board_dispatch.create_post ~author:"reaction-author"
      ~content:"reactable post" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let post_id = Board.Post_id.to_string post.id in
      (match
         Board_dispatch.toggle_reaction ~target_type:Board.Reaction_post
           ~target_id:post_id ~user_id:"reactor" ~emoji:"🚀"
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok result ->
           Alcotest.(check bool) "reacted" true result.reacted;
           Alcotest.(check int) "summary length" 1 (List.length result.summary);
           (match result.summary with
            | summary :: _ ->
                Alcotest.(check string) "summary emoji" "🚀" summary.emoji;
                Alcotest.(check int) "summary count" 1 summary.count;
                Alcotest.(check bool) "summary reacted" true summary.reacted;
                Alcotest.(check (list string)) "summary recent users"
                  [ "reactor" ] summary.recent_user_ids
            | [] -> Alcotest.fail "expected reaction summary"));
      (match
         Board_dispatch.list_reactions ~target_type:Board.Reaction_post
           ~target_id:post_id ~user_id:"reactor" ()
       with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok summaries ->
           Alcotest.(check int) "listed summary length" 1
             (List.length summaries));
      (match
         Board_dispatch.toggle_reaction ~target_type:Board.Reaction_post
           ~target_id:post_id ~user_id:"reactor" ~emoji:"🚀"
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok result ->
           Alcotest.(check bool) "reacted after untoggle" false
             result.reacted;
           Alcotest.(check int) "summary empty after untoggle" 0
             (List.length result.summary))

let test_reaction_persistence_failure_returns_error_without_publish () =
  let post =
    match
      Board_dispatch.create_post
        ~author:"reaction-author"
        ~content:"reaction persistence boundary"
        ~post_kind:Board.Human_post
        ()
    with
    | Ok post -> post
    | Error error -> Alcotest.fail (Board.show_board_error error)
  in
  let post_id = Board.Post_id.to_string post.id in
  let keeper_signals = ref 0 in
  let sse_reactions = ref 0 in
  Board_dispatch.set_board_signal_hook (fun _ -> incr keeper_signals);
  Board_dispatch.set_board_sse_hook (function
    | Board_dispatch.Reaction_changed _ -> incr sse_reactions
    | _ -> ());
  ignore (block_board_masc_dir_with_file ());
  let before_errors = Board.persist_error_count () in
  check_io_error
    ~where:"rewrite_reactions"
    (Board_dispatch.toggle_reaction
       ~target_type:Board.Reaction_post
       ~target_id:post_id
       ~user_id:"reactor"
       ~emoji:"🚀");
  Alcotest.(check bool)
    "persist error counter incremented"
    true
    (Board.persist_error_count () > before_errors);
  Alcotest.(check int) "Keeper signal not published" 0 !keeper_signals;
  Alcotest.(check int) "SSE reaction not published" 0 !sse_reactions;
  match
    Board_dispatch.list_reactions
      ~target_type:Board.Reaction_post
      ~target_id:post_id
      ~user_id:"reactor"
      ()
  with
  | Error error -> Alcotest.fail (Board.show_board_error error)
  | Ok summaries ->
    Alcotest.(check int) "failed reaction stays absent in memory" 0 (List.length summaries)
;;

let test_comment_reaction_survives_restart () =
  match
    Board_dispatch.create_post ~author:"reaction-author"
      ~content:"comment reaction parent" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let post_id = Board.Post_id.to_string post.id in
      let comment_id =
        match
          Board_dispatch.add_comment ~post_id ~author:"commenter"
            ~content:"reactable comment" ()
        with
        | Error e -> Alcotest.fail (Board.show_board_error e)
        | Ok comment -> Board.Comment_id.to_string comment.id
      in
      (match
         Board_dispatch.toggle_reaction ~target_type:Board.Reaction_comment
           ~target_id:comment_id ~user_id:"reactor" ~emoji:"👏"
       with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok _ -> ());
      Board.reset_global_for_test ();
      Board_dispatch.reset_for_test ();
      Board_dispatch.init_jsonl ();
      match
        Board_dispatch.list_reactions ~target_type:Board.Reaction_comment
          ~target_id:comment_id ~user_id:"reactor" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok summaries ->
          Alcotest.(check int) "restored summary length" 1
            (List.length summaries);
          match summaries with
          | summary :: _ ->
              Alcotest.(check string) "restored emoji" "👏" summary.emoji;
              Alcotest.(check int) "restored count" 1 summary.count;
              Alcotest.(check bool) "restored reacted" true summary.reacted;
              Alcotest.(check (list string)) "restored recent users"
                [ "reactor" ] summary.recent_user_ids
          | [] -> Alcotest.fail "expected restored reaction summary"

let test_reaction_summary_recent_user_ids () =
  match
    Board_dispatch.create_post ~author:"reaction-author"
      ~content:"recent reactors parent" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let post_id = Board.Post_id.to_string post.id in
      List.iter
        (fun user_id ->
           match
             Board_dispatch.toggle_reaction ~target_type:Board.Reaction_post
               ~target_id:post_id ~user_id ~emoji:"👍"
           with
           | Ok _ -> ()
           | Error e -> Alcotest.fail (Board.show_board_error e))
        [ "reactor-a"; "reactor-b"; "reactor-c" ];
      match
        Board_dispatch.list_reactions ~target_type:Board.Reaction_post
          ~target_id:post_id ~user_id:"reactor-b" ()
      with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok summaries ->
          match summaries with
          | summary :: _ ->
              Alcotest.(check string) "summary emoji" "👍" summary.emoji;
              Alcotest.(check int) "summary count" 3 summary.count;
              Alcotest.(check bool) "selected user reacted" true
                summary.reacted;
              Alcotest.(check int) "recent user cap input length" 3
                (List.length summary.recent_user_ids);
              List.iter
                (fun user_id ->
                   Alcotest.(check bool)
                     (Printf.sprintf "recent includes %s" user_id)
                     true
                     (List.mem user_id summary.recent_user_ids))
                [ "reactor-a"; "reactor-b"; "reactor-c" ]
          | [] -> Alcotest.fail "expected reaction summary"

let test_reaction_summary_batch () =
  match
    Board_dispatch.create_post ~author:"reaction-author"
      ~content:"batch reactors parent" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let post_id = Board.Post_id.to_string post.id in
      let comment_id =
        match
          Board_dispatch.add_comment ~post_id ~author:"commenter"
            ~content:"batch reaction child" ()
        with
        | Error e -> Alcotest.fail (Board.show_board_error e)
        | Ok comment -> Board.Comment_id.to_string comment.id
      in
      List.iter
        (fun (target_type, target_id, user_id, emoji) ->
           match
             Board_dispatch.toggle_reaction ~target_type ~target_id ~user_id
               ~emoji
           with
           | Ok _ -> ()
           | Error e -> Alcotest.fail (Board.show_board_error e))
        [
          (Board.Reaction_post, post_id, "reactor-a", "👍");
          (Board.Reaction_post, post_id, "reactor-b", "👍");
          (Board.Reaction_comment, comment_id, "reactor-b", "👏");
        ];
      let rows =
        Board_dispatch.list_reactions_batch
          ~targets:
            [
              (Board.Reaction_post, post_id);
              (Board.Reaction_comment, comment_id);
            ]
          ~user_id:"reactor-b" ()
      in
      let summaries_for target =
        List.assoc_opt target rows |> Option.value ~default:[]
      in
      (match summaries_for (Board.Reaction_post, post_id) with
       | summary :: _ ->
           Alcotest.(check string) "post emoji" "👍" summary.emoji;
           Alcotest.(check int) "post count" 2 summary.count;
           Alcotest.(check bool) "post reacted" true summary.reacted
       | [] -> Alcotest.fail "expected post reaction summary");
      (match summaries_for (Board.Reaction_comment, comment_id) with
       | summary :: _ ->
           Alcotest.(check string) "comment emoji" "👏" summary.emoji;
           Alcotest.(check int) "comment count" 1 summary.count;
           Alcotest.(check bool) "comment reacted" true summary.reacted
       | [] -> Alcotest.fail "expected comment reaction summary")

let test_dashboard_detail_uses_authenticated_reaction_actor () =
  let post =
    match
      Board_dispatch.create_post
        ~author:"reaction-author"
        ~content:"credential-bound reaction state"
        ~post_kind:Board.Human_post
        ()
    with
    | Ok post -> post
    | Error error -> Alcotest.fail (Board.show_board_error error)
  in
  let post_id = Board.Post_id.to_string post.id in
  (match
     Board_dispatch.toggle_reaction
       ~target_type:Board.Reaction_post
       ~target_id:post_id
       ~user_id:"credential-owner"
       ~emoji:"👍"
   with
   | Ok _ -> ()
   | Error error -> Alcotest.fail (Board.show_board_error error));
  let status, body =
    Server_routes_http_runtime.board_post_detail_json
      ~config:None
      ~voter:(Some "forgeable-query-voter")
      ~reaction_actor:(Some "credential-owner")
      ~response_format:Server_board_post_response_format.Flat
      ~post_id
  in
  Alcotest.(check bool) "detail status" true (status = `OK);
  let open Yojson.Safe.Util in
  let summary =
    Yojson.Safe.from_string body
    |> member "reactions"
    |> to_list
    |> List.find_opt (fun json -> String.equal (json |> member "emoji" |> to_string) "👍")
  in
  match summary with
  | None -> Alcotest.fail "expected thumbs-up reaction summary"
  | Some summary ->
    Alcotest.(check bool)
      "reacted is bound to authenticated credential, not query voter"
       true
       (summary |> member "reacted" |> to_bool)

let test_board_post_response_format_query_contract () =
  let check_format label expected query =
    match Server_board_post_response_format.of_query query with
    | Error _ -> Alcotest.fail (label ^ ": expected a supported format")
    | Ok actual ->
      Alcotest.(check string)
        label
        (Server_board_post_response_format.to_wire expected)
        (Server_board_post_response_format.to_wire actual)
  in
  check_format
    "missing format defaults to nested"
    Server_board_post_response_format.Nested
    None;
  check_format
    "flat format is normalized at the boundary"
    Server_board_post_response_format.Flat
    (Some " FLAT ");
  match Server_board_post_response_format.of_query (Some "yaml") with
  | Ok _ -> Alcotest.fail "unsupported format must be rejected"
  | Error error ->
    let open Yojson.Safe.Util in
    let json = Server_board_post_response_format.error_json error in
    Alcotest.(check string)
      "typed error code"
      "unsupported_board_post_response_format"
      (json |> member "code" |> to_string)

let test_board_sse_reaction_changed () =
  let post_id =
    match
      Board_dispatch.create_post ~author:"reaction-author"
        ~content:"sse reaction parent" ~post_kind:Board.Human_post ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> Board.Post_id.to_string post.id
  in
  let seen = ref None in
  Board_dispatch.set_board_sse_hook (fun event -> seen := Some event);
  (match
     Board_dispatch.toggle_reaction ~target_type:Board.Reaction_post
       ~target_id:post_id ~user_id:"reactor" ~emoji:"🔥"
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  match !seen with
  | Some
      (Board_dispatch.Reaction_changed
         { target_type; target_id; user_id; emoji; reacted }) ->
      Alcotest.(check string) "target type" "post"
        (Board.reaction_target_type_to_string target_type);
      Alcotest.(check string) "target id" post_id target_id;
      Alcotest.(check string) "user" "reactor" user_id;
      Alcotest.(check string) "emoji" "🔥" emoji;
      Alcotest.(check bool) "reacted" true reacted
  | Some _ -> Alcotest.fail "expected reaction_changed SSE event"
  | None -> Alcotest.fail "expected board SSE event"

let test_board_signal_reaction_changed_resolves_comment_parent () =
  let post =
    match
      Board_dispatch.create_post ~author:"reaction-author"
        ~title:"Reaction parent title"
        ~content:"reaction parent content @keeper-alpha"
        ~post_kind:Board.Human_post ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  let comment =
    match
      Board_dispatch.add_comment ~post_id ~author:"comment-author"
        ~content:"comment target" ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok comment -> comment
  in
  let comment_id = Board.Comment_id.to_string comment.id in
  let seen = ref None in
  Board_dispatch.set_board_signal_hook (fun signal -> seen := Some signal);
  (match
     Board_dispatch.toggle_reaction ~target_type:Board.Reaction_comment
       ~target_id:comment_id ~user_id:"reactor" ~emoji:"👏"
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  match !seen with
  | Some addressed ->
    let signal = addressed.Board_dispatch.signal in
    (match signal.Board_dispatch.kind with
     | Board_dispatch.Board_reaction_changed
         { target_type; target_id; user_id; emoji; reacted } ->
      Alcotest.(check string) "parent post id" post_id signal.Board_dispatch.post_id;
      Alcotest.(check string) "signal author is reactor" "reactor" signal.author;
      Alcotest.(check string) "target type" "comment"
        (Board.reaction_target_type_to_string target_type);
      Alcotest.(check string) "target id" comment_id target_id;
      Alcotest.(check string) "user" "reactor" user_id;
      Alcotest.(check string) "emoji" "👏" emoji;
      Alcotest.(check bool) "reacted" true reacted;
      Alcotest.(check string) "parent title" "Reaction parent title" signal.title;
      Alcotest.(check string) "parent content" "reaction parent content @keeper-alpha" signal.content
      ; (match addressed.audience with
         | Board.Thread_participants -> ()
         | _ -> Alcotest.fail "reaction audience must be thread participants")
     | _ -> Alcotest.fail "expected reaction_changed board signal")
  | None -> Alcotest.fail "expected reaction_changed board signal"

(* #29457: a vote is a board signal addressed at the voted-on author. The
   signal's [author] is the voter (the actor) and the payload carries whose
   writing was voted on, so the keeper router can wake that lane directly. *)
let vote_signals_seen () =
  let seen = ref [] in
  Board_dispatch.set_board_signal_hook (fun signal -> seen := signal :: !seen);
  fun () -> List.rev !seen

let test_board_signal_vote_cast_on_post () =
  let post =
    match
      Board_dispatch.create_post ~author:"vote-author" ~title:"Vote parent"
        ~content:"vote parent content" ~post_kind:Board.Human_post ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  let signals = vote_signals_seen () in
  (match Board_dispatch.vote ~voter:"voter-one" ~post_id ~direction:Board.Up with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  (match signals () with
   | [ addressed ] ->
     let signal = addressed.Board_dispatch.signal in
     (match signal.Board_dispatch.kind with
      | Board_dispatch.Board_vote_cast { target; target_author; voter; direction } ->
        (match target with
         | Board_dispatch.Vote_on_post id ->
           Alcotest.(check string) "target is the post" post_id id
         | Board_dispatch.Vote_on_comment _ -> Alcotest.fail "expected a post target");
        Alcotest.(check string) "target author" "vote-author" target_author;
        Alcotest.(check string) "voter" "voter-one" voter;
        Alcotest.(check string) "direction" "up"
          (Board.vote_direction_to_string direction);
        Alcotest.(check string) "signal author is the voter" "voter-one" signal.author;
        Alcotest.(check string) "signal post" post_id signal.Board_dispatch.post_id;
        Alcotest.(check string) "signal title" "Vote parent" signal.title;
        (match addressed.audience with
         | Board.Thread_participants -> ()
         | _ -> Alcotest.fail "vote audience must be thread participants")
      | _ -> Alcotest.fail "expected vote_cast board signal")
   | signals ->
     Alcotest.failf "expected exactly one board signal, got %d" (List.length signals));
  (* A same-direction duplicate is Already_voted: no store change, no signal. *)
  (match Board_dispatch.vote ~voter:"voter-one" ~post_id ~direction:Board.Up with
   | Error (Board.Already_voted _) -> ()
   | Ok _ -> Alcotest.fail "expected Already_voted"
   | Error e -> Alcotest.fail (Board.show_board_error e));
  Alcotest.(check int) "duplicate vote emits no signal" 1 (List.length (signals ()))

let test_board_signal_vote_cast_on_comment_names_comment_author () =
  let post =
    match
      Board_dispatch.create_post ~author:"vote-post-author" ~title:"Comment vote parent"
        ~content:"comment vote parent content" ~post_kind:Board.Human_post ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  let comment =
    match
      Board_dispatch.add_comment ~post_id ~author:"vote-comment-author"
        ~content:"comment vote target" ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok comment -> comment
  in
  let comment_id = Board.Comment_id.to_string comment.id in
  let signals = vote_signals_seen () in
  (match
     Board_dispatch.vote_comment ~voter:"voter-two" ~comment_id ~direction:Board.Down
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  match signals () with
  | [ addressed ] ->
    let signal = addressed.Board_dispatch.signal in
    (match signal.Board_dispatch.kind with
     | Board_dispatch.Board_vote_cast { target; target_author; voter; direction } ->
       (match target with
        | Board_dispatch.Vote_on_comment id ->
          Alcotest.(check string) "target is the comment" comment_id id
        | Board_dispatch.Vote_on_post _ -> Alcotest.fail "expected a comment target");
       Alcotest.(check string) "target author is the commenter" "vote-comment-author"
         target_author;
       Alcotest.(check string) "voter" "voter-two" voter;
       Alcotest.(check string) "direction" "down"
         (Board.vote_direction_to_string direction);
       Alcotest.(check string) "signal post is the parent" post_id
         signal.Board_dispatch.post_id
     | _ -> Alcotest.fail "expected vote_cast board signal")
  | signals ->
    Alcotest.failf "expected exactly one board signal, got %d" (List.length signals)

let test_reaction_rejects_unsupported_emoji () =
  match
    Board_dispatch.create_post ~author:"reaction-author"
      ~content:"unsupported reaction parent" ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let post_id = Board.Post_id.to_string post.id in
      match
        Board_dispatch.toggle_reaction ~target_type:Board.Reaction_post
          ~target_id:post_id ~user_id:"reactor" ~emoji:"😄"
      with
      | Ok _ -> Alcotest.fail "expected Validation_error"
      | Error (Board.Validation_error _) -> ()
      | Error e -> Alcotest.fail (Board.show_board_error e)

(** {1 Stats / Search / Hearth} *)

let test_stats () =
  let stats = Board_dispatch.stats () in
  match stats with
  | `Assoc fields ->
      Alcotest.(check bool) "has post_count"
        true (List.mem_assoc "post_count" fields)
  | _ -> Alcotest.fail "stats should be JSON object"

let test_search () =
  ignore (Board_dispatch.create_post ~author:"searcher"
            ~content:"unique_dispatch_search_term" ~post_kind:Board.Human_post ());
  let results = Board_dispatch.search ~query:"unique_dispatch_search_term" ~limit:10 in
  Alcotest.(check bool) "found search result" true (List.length results >= 1)

let test_hearths () =
  ignore (Board_dispatch.create_post ~author:"hearth-test" ~content:"fire topic"
    ~hearth:"test-hearth" ~post_kind:Board.Human_post ());
  ignore (Board_dispatch.create_post ~author:"hearth-system" ~content:"system topic"
    ~hearth:"system-hearth" ~post_kind:Board.System_post ());
  ignore (Board_dispatch.create_post ~author:"hearth-automation" ~content:"automation topic"
    ~hearth:"automation-hearth" ~post_kind:Board.Automation_post ());
  let hearths = Board_dispatch.list_hearths () in
  let count name rows = List.assoc_opt name rows |> Option.value ~default:0 in
  Alcotest.(check bool) "has hearths" true (List.length hearths >= 3);
  Alcotest.(check int) "unfiltered system count" 1 (count "system-hearth" hearths);
  Alcotest.(check int) "unfiltered automation count" 1 (count "automation-hearth" hearths);
  let without_system = Board_dispatch.list_hearths ~exclude_system:true () in
  Alcotest.(check int) "system excluded" 0 (count "system-hearth" without_system);
  Alcotest.(check int) "automation retained" 1 (count "automation-hearth" without_system);
  let direct_only =
    Board_dispatch.list_hearths ~exclude_system:true ~exclude_automation:true ()
  in
  Alcotest.(check int) "automation excluded" 0 (count "automation-hearth" direct_only);
  Alcotest.(check int) "human retained" 1 (count "test-hearth" direct_only)

let test_set_thread_id () =
  match
    Board_dispatch.create_post ~author:"thread-test" ~content:"link me"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      match Board_dispatch.set_thread_id ~post_id:pid ~thread_id:"thread-abc" with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok () ->
          match Board_dispatch.get_post ~post_id:pid with
          | Error e -> Alcotest.fail (Board.show_board_error e)
          | Ok p ->
              Alcotest.(check (option string)) "thread_id set"
                (Some "thread-abc") p.thread_id

let test_set_pinned () =
  match
    Board_dispatch.create_post ~author:"pin-test" ~content:"pin me"
      ~post_kind:Board.Human_post ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post ->
      let pid = Board.Post_id.to_string post.id in
      Alcotest.(check bool) "post starts unpinned" false post.pinned;
      (match Board_dispatch.set_pinned ~post_id:pid ~pinned:true with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok () -> ());
      (match Board_dispatch.get_post ~post_id:pid with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok p -> Alcotest.(check bool) "pinned set in memory" true p.pinned);
      (* set_pinned marks the post dirty, so the flag must survive a restart
         (unlike set_thread_id which is in-memory only). *)
      Board.reset_global_for_test ();
      Board_dispatch.reset_for_test ();
      Board_dispatch.init_jsonl ();
      (match Board_dispatch.get_post ~post_id:pid with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok p -> Alcotest.(check bool) "pinned survives restart" true p.pinned);
      (match Board_dispatch.set_pinned ~post_id:pid ~pinned:false with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok () -> ());
      match Board_dispatch.get_post ~post_id:pid with
      | Error e -> Alcotest.fail (Board.show_board_error e)
      | Ok p -> Alcotest.(check bool) "unpinned after toggle off" false p.pinned

let test_set_pinned_missing_post () =
  match Board_dispatch.set_pinned ~post_id:"never-existed" ~pinned:true with
  | Ok () -> Alcotest.fail "expected Post_not_found"
  | Error (Board.Post_not_found _) -> ()
  | Error e -> Alcotest.fail (Board.show_board_error e)

let test_set_pinned_persistence_failure_rolls_back () =
  let post =
    match
      Board_dispatch.create_post
        ~author:"pin-persist-fail-author"
        ~content:"pin must roll back on failed append"
        ~post_kind:Board.Human_post
        ()
    with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> post
  in
  let post_id = Board.Post_id.to_string post.id in
  ignore (block_board_masc_dir_with_file ());
  let before_errors = Board.persist_error_count () in
  check_io_error
    ~where:"append_post"
    (Board_dispatch.set_pinned ~post_id ~pinned:true);
  Alcotest.(check bool)
    "persist error counter incremented"
    true
    (Board.persist_error_count () > before_errors);
  match Board_dispatch.get_post ~post_id with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok fetched ->
      Alcotest.(check bool) "pinned rolled back" false fetched.pinned

(** {1 Validation} *)

let test_empty_content () =
  match
    Board_dispatch.create_post ~author:"validator" ~content:""
      ~post_kind:Board.Human_post ()
  with
  | Ok _ -> Alcotest.fail "Expected validation error for empty content"
  | Error (Board.Validation_error _) -> ()
  | Error e -> Alcotest.fail (Board.show_board_error e)

let test_invalid_author () =
  match
    Board_dispatch.create_post ~author:"" ~content:"valid content"
      ~post_kind:Board.Human_post ()
  with
  | Ok _ -> Alcotest.fail "Expected validation error for empty author"
  | Error _ -> ()

let expect_validation_error label = function
  | Error (Board.Validation_error _) -> ()
  | Error error -> Alcotest.failf "%s: %s" label (Board.show_board_error error)
  | Ok _ -> Alcotest.failf "%s: expected Validation_error" label
;;

let test_direct_post_requires_exact_targets () =
  expect_validation_error
    "targetless Direct post"
    (Board_dispatch.create_post
       ~author:"validator"
       ~content:"private but unaddressed"
       ~visibility:Board.Direct
       ~post_kind:Board.Human_post
       ());
  expect_validation_error
    "Direct broadcast"
    (Board_dispatch.create_post
       ~author:"validator"
       ~content:"@@all private broadcast is contradictory"
       ~visibility:Board.Direct
       ~post_kind:Board.Human_post
       ())
;;

let audience_label = function
  | Board.Targets _ -> "targets"
  | Board.Broadcast -> "broadcast"
  | Board.Thread_participants -> "thread_participants"
  | Board.Discoverable -> "discoverable"
;;

(* [Agent_id.of_string] checks shape only — 1..64 of [a-zA-Z0-9._-] with one
   optional colon — so a candidate it rejects fails on a character or a length
   no keeper name can have. [board_audience.ml] reads those as prose rather
   than as an address that went wrong, and records what the rejection was
   actually catching in the live log: a bare "@" five times, "@@" three times,
   and "@internals/libs/errors/asyncErrorHandler." once.

   A plausible misspelling ("@alcie" for "@alice") passes the shape check and
   is still lost on the read side; that gap needs the keeper registry at the
   write boundary and is not what this pins. *)
let test_malformed_target_is_read_as_prose () =
  Alcotest.(check string)
    "a token no Agent_id can be is prose, not a failed address"
    "discoverable"
    (match
       Board.audience_for_post
         ~visibility:Board.Public
         ~title:""
         ~content:"please inspect @!"
     with
     | Ok audience -> audience_label audience
     | Error error -> Board.show_board_error error)
;;

let test_prose_mentions_do_not_record_validation_rejections () =
  (* Reading an old comment must not replay security diagnostics for text
     which is not an identity. This token was observed in live Board prose. *)
  let content = "please inspect @check·lint and @internals/libs/errors" in
  let before = Validation.get_rejection_stats () in
  List.iter
    (fun _ ->
       (match Board.audience_for_comment ~content with
        | Ok Board.Thread_participants -> ()
        | Ok _ -> Alcotest.fail "prose unexpectedly addressed a keeper"
        | Error error -> Alcotest.fail (Board.show_board_error error));
       Alcotest.(check (pair int (float 0.0)))
         "repeated prose reads preserve rejection count and timestamp"
         before (Validation.get_rejection_stats ()))
    [ (); (); () ];
  (match
     Board.audience_for_comment
       ~content:"@check·lint @edgar.a.poe @keeper:MiXeD.Agent"
   with
   | Ok (Board.Targets targets) ->
     Alcotest.(check (list string)) "valid identities survive rejected prose"
       [ "edgar.a.poe"; "keeper:MiXeD.Agent" ]
       (List.map Board.Agent_id.to_string targets)
   | Ok _ -> Alcotest.fail "valid mentions lost their target audience"
   | Error error -> Alcotest.fail (Board.show_board_error error));
  Alcotest.(check (pair int (float 0.0)))
    "mixed mentions preserve rejection statistics"
    before (Validation.get_rejection_stats ());
  expect_validation_error "invalid external author"
    (Board_dispatch.create_post ~author:"check·lint" ~content:"valid content"
       ~post_kind:Board.Human_post ());
  let count, timestamp = Validation.get_rejection_stats () in
  Alcotest.(check int) "external identity rejection remains observable"
    (fst before + 1) count;
  Alcotest.(check bool) "external rejection has a timestamp" true
    (timestamp > 0.0)
;;

(* Unlisted means "not in feeds, but accessible", and a keeper's feed is its
   attention collector: an unaddressed Unlisted post is thread activity, so
   no keeper pays an attention judgment for it. Verification verdict receipts
   are written this way. An explicit address or @@all still wins, and an
   Internal post stays discoverable. *)
let test_unlisted_post_is_not_discoverable () =
  let label ~visibility ~content =
    match Board.audience_for_post ~visibility ~title:"" ~content with
    | Ok audience -> audience_label audience
    | Error error -> Board.show_board_error error
  in
  Alcotest.(check string)
    "unaddressed unlisted post is thread activity"
    "thread_participants"
    (label ~visibility:Board.Unlisted ~content:"Approved task task-1 (vrf:v1)");
  Alcotest.(check string)
    "addressed unlisted post keeps its targets"
    "targets"
    (label ~visibility:Board.Unlisted ~content:"@keeper-a Approved task task-1");
  Alcotest.(check string)
    "@@all on an unlisted post still broadcasts"
    "broadcast"
    (label ~visibility:Board.Unlisted ~content:"@@all read this");
  Alcotest.(check string)
    "unaddressed internal post stays discoverable"
    "discoverable"
    (label ~visibility:Board.Internal ~content:"Stalled task task-1 (vrf:v1)")
;;

let test_write_boundary_emits_typed_audience () =
  let seen = ref None in
  Board_dispatch.set_board_signal_hook (fun addressed -> seen := Some addressed);
  (match
     Board_dispatch.create_post
       ~author:"validator"
       ~content:"@MiXeD-Agent inspect this"
       ~post_kind:Board.Human_post
       ()
   with
   | Ok _ -> ()
   | Error error -> Alcotest.fail (Board.show_board_error error));
  match !seen with
  | None -> Alcotest.fail "typed Board signal was not emitted"
  | Some addressed ->
    (match addressed.Board_dispatch.audience with
     | Board.Targets [ target ] ->
       Alcotest.(check string)
         "Board target retains the typed source identity"
         "MiXeD-Agent"
         (Board.Agent_id.to_string target)
     | _ -> Alcotest.fail "expected one exact Board target")
;;

let test_target_case_is_identity_level () =
  (* #25601: the shared grammar (Board_addressing) preserves target case;
     case normalization is an identity-level concern.  Board agent ids are
     case-sensitive, so differently-cased targets stay DISTINCT here —
     while the Keeper boundary folds them into one canonical id (pinned in
     test_keeper_lane_mentions). *)
  match
    Board.audience_for_post ~visibility:Board.Public ~title:""
      ~content:"@MiXeD-Agent and @mixed-agent"
  with
  | Ok (Board.Targets targets) ->
    Alcotest.(check (list string))
      "Board targets keep distinct casings"
      [ "MiXeD-Agent"; "mixed-agent" ]
      (List.map Board.Agent_id.to_string targets)
  | Ok _ -> Alcotest.fail "expected exact targets"
  | Error error -> Alcotest.fail (Board.show_board_error error)
;;

(** {1 SubBoard CRUD} *)

let test_sub_board_create_and_get () =
  (match Board_dispatch.create_sub_board ~slug:"alpha-board" ~name:"Alpha"
           ~description:"First sub-board" ~owner:"agent-1" () with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok sb ->
      Alcotest.(check string) "slug matches" "alpha-board" sb.Board.slug;
      Alcotest.(check string) "name matches" "Alpha" sb.name;
      Alcotest.(check string) "owner matches" "agent-1"
        (Board.Agent_id.to_string sb.owner);
      let id = Board.Sub_board_id.to_string sb.id in
      (match Board_dispatch.get_sub_board ~sub_board_id:id with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok fetched ->
           Alcotest.(check string) "fetched id" id (Board.Sub_board_id.to_string fetched.id)));
  (* lookup by slug *)
  (match Board_dispatch.get_sub_board ~sub_board_id:"alpha-board" with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok fetched ->
       Alcotest.(check string) "slug lookup" "alpha-board" fetched.Board.slug)

let test_sub_board_list () =
  ignore (Board_dispatch.create_sub_board ~slug:"list-a" ~name:"A" ~description:""
            ~owner:"agent-1" ());
  ignore (Board_dispatch.create_sub_board ~slug:"list-b" ~name:"B" ~description:""
            ~owner:"agent-2" ());
  let all = Board_dispatch.list_sub_boards () in
  Alcotest.(check bool) "has sub-boards" true (List.length all >= 2)

let test_sub_board_slug_conflict () =
  ignore (Board_dispatch.create_sub_board ~slug:"conflict-slug" ~name:"First"
            ~description:"" ~owner:"agent-1" ());
  (match Board_dispatch.create_sub_board ~slug:"conflict-slug" ~name:"Second"
           ~description:"" ~owner:"agent-2" () with
  | Error Board.Already_exists _ -> ()  (* expected *)
  | Ok _ -> Alcotest.fail "expected Already_exists for duplicate slug"
  | Error e -> Alcotest.fail ("unexpected error: " ^ Board.show_board_error e))

let test_sub_board_delete () =
  (match Board_dispatch.create_sub_board ~slug:"delete-me" ~name:"Temp"
           ~description:"" ~owner:"agent-1" () with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok sb ->
      let id = Board.Sub_board_id.to_string sb.Board.id in
      (match Board_dispatch.delete_sub_board ~sub_board_id:id with
       | Error e -> Alcotest.fail (Board.show_board_error e)
       | Ok () ->
           (match Board_dispatch.get_sub_board ~sub_board_id:id with
            | Error (Board.Invalid_id _) -> ()  (* expected after delete *)
            | Ok _ -> Alcotest.fail "expected not found after delete"
            | Error e -> Alcotest.fail (Board.show_board_error e))))

let sub_board_slugs_from_disk () =
  let path = Board.sub_boards_path () in
  if not (Fs_compat.file_exists path) then []
  else
    Fs_compat.load_jsonl path
    |> List.filter_map (fun json ->
           match Yojson.Safe.Util.member "slug" json with
           | `String slug -> Some slug
           | _ -> None)

let test_sub_board_create_delete_persisted_snapshot () =
  let created =
    Board_dispatch.create_sub_board ~slug:"persisted-team" ~name:"Persisted"
      ~description:"" ~owner:"agent-1" ()
  in
  let id =
    match created with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok sb -> Board.Sub_board_id.to_string sb.Board.id
  in
  Alcotest.(check bool) "created slug persisted" true
    (List.exists (String.equal "persisted-team") (sub_board_slugs_from_disk ()));
  (match Board_dispatch.delete_sub_board ~sub_board_id:id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok () -> ());
  Alcotest.(check bool) "deleted slug removed from persisted snapshot" false
    (List.exists (String.equal "persisted-team") (sub_board_slugs_from_disk ()))

let test_sub_board_access_default_open () =
  (match Board_dispatch.create_sub_board ~slug:"open-board" ~name:"Open"
           ~description:"" ~owner:"agent-1" () with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok sb ->
      Alcotest.(check bool) "default access is Open"
        true (sb.Board.access = Board.Open))

let test_sub_board_members_include_owner () =
  (match Board_dispatch.create_sub_board ~slug:"member-board" ~name:"Members"
           ~description:"" ~owner:"agent-owner"
           ~members:[" member-a "; "agent-owner"; "member-a"]
           ~access:Board.Members_only () with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok sb ->
       let members = List.map Board.Agent_id.to_string sb.Board.members in
       Alcotest.(check (list string)) "owner first, members deduped"
         ["agent-owner"; "member-a"] members)

let test_sub_board_members_only_post_policy () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"policy-team" ~name:"Policy"
       ~description:"" ~owner:"agent-owner" ~members:["agent-member"]
       ~access:Board.Members_only ());
  (match
     Board_dispatch.create_post ~author:"agent-outsider" ~content:"nope"
       ~post_kind:Board.Human_post ~hearth:"policy-team" ()
   with
   | Error (Board.Validation_error _) -> ()
   | Error e -> Alcotest.fail ("unexpected error: " ^ Board.show_board_error e)
   | Ok _ -> Alcotest.fail "expected members-only sub-board to reject outsider");
  (match
     Board_dispatch.create_post ~author:"agent-member" ~content:"allowed"
       ~post_kind:Board.Human_post ~hearth:" POLICY-TEAM " ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  (match
     Board_dispatch.create_post ~author:"agent-owner" ~content:"owner allowed"
       ~post_kind:Board.Human_post ~hearth:"policy-team" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ())

let test_sub_board_owner_only_post_policy () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"owner-space" ~name:"Owner"
       ~description:"" ~owner:"agent-owner" ~members:["agent-member"]
       ~access:Board.Owner_only ());
  (match
     Board_dispatch.create_post ~author:"agent-member" ~content:"member denied"
       ~post_kind:Board.Human_post ~hearth:"owner-space" ()
   with
   | Error (Board.Validation_error _) -> ()
   | Error e -> Alcotest.fail ("unexpected error: " ^ Board.show_board_error e)
   | Ok _ -> Alcotest.fail "expected owner-only sub-board to reject member");
  (match
     Board_dispatch.create_post ~author:"agent-owner" ~content:"owner allowed"
       ~post_kind:Board.Human_post ~hearth:"owner-space" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ())

(* Issue #16024 / task-314: PR #13490 enforced sub-board policy for
   [create_post] but the matching gate in [add_comment] was missing,
   so a non-member could comment on a Members_only sub-board through
   any post that already lived in that sub-board.  These tests pin the
   contract: comment policy mirrors post policy on the parent post's
   hearth. *)

let post_in_sub_board ~author ~slug ~content =
  match
    Board_dispatch.create_post ~author ~content
      ~post_kind:Board.Human_post ~hearth:slug ()
  with
  | Error e -> Alcotest.fail (Board.show_board_error e)
  | Ok post -> Board.Post_id.to_string post.id

let test_sub_board_members_only_comment_policy () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"comment-policy-team"
       ~name:"CommentPolicy" ~description:"" ~owner:"agent-owner"
       ~members:["agent-member"] ~access:Board.Members_only ());
  let post_id =
    post_in_sub_board ~author:"agent-owner"
      ~slug:"comment-policy-team" ~content:"seed post"
  in
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-outsider"
       ~content:"outsider reply" ()
   with
   | Error (Board.Validation_error _) -> ()
   | Error e ->
     Alcotest.fail ("unexpected error: " ^ Board.show_board_error e)
   | Ok _ ->
     Alcotest.fail
       "expected members-only sub-board to reject outsider comment");
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-member"
       ~content:"member reply" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ());
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-owner"
       ~content:"owner reply" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ())

let test_sub_board_owner_only_comment_policy () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"comment-owner-space"
       ~name:"CommentOwner" ~description:"" ~owner:"agent-owner"
       ~members:["agent-member"] ~access:Board.Owner_only ());
  let post_id =
    post_in_sub_board ~author:"agent-owner"
      ~slug:"comment-owner-space" ~content:"seed"
  in
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-member"
       ~content:"member reply" ()
   with
   | Error (Board.Validation_error _) -> ()
   | Error e ->
     Alcotest.fail ("unexpected error: " ^ Board.show_board_error e)
   | Ok _ ->
     Alcotest.fail
       "expected owner-only sub-board to reject member comment");
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-owner"
       ~content:"owner reply" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ())

let test_sub_board_open_comment_policy_allows_anyone () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"comment-open"
       ~name:"CommentOpen" ~description:"" ~owner:"agent-owner"
       ~access:Board.Open ());
  let post_id =
    post_in_sub_board ~author:"agent-owner"
      ~slug:"comment-open" ~content:"seed"
  in
  (match
     Board_dispatch.add_comment ~post_id ~author:"agent-random"
       ~content:"random reply" ()
   with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok _ -> ())

let test_sub_board_update () =
  let id =
    match Board_dispatch.create_sub_board ~slug:"update-target" ~name:"Before"
             ~description:"old desc" ~owner:"agent-owner" ~access:Board.Open () with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok sb -> Board.Sub_board_id.to_string sb.Board.id
  in
  (match Board_dispatch.update_sub_board ~sub_board_id:id ~name:"After"
           ~description:"new desc" ~access:Board.Members_only ~members:["agent-a"] () with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok sb ->
       Alcotest.(check string) "updated name" "After" sb.Board.name;
       Alcotest.(check string) "updated description" "new desc" sb.description;
       Alcotest.(check bool) "updated access" true (sb.access = Board.Members_only);
       let members = List.map Board.Agent_id.to_string sb.members in
       Alcotest.(check (list string)) "updated members include owner" ["agent-owner"; "agent-a"] members);
  (* lookup still works after update *)
  (match Board_dispatch.get_sub_board ~sub_board_id:id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok sb ->
       Alcotest.(check string) "persisted name" "After" sb.Board.name)

let test_sub_board_delete_clears_orphan_hearth () =
  let sb_id =
    match Board_dispatch.create_sub_board ~slug:"orphan-hearth" ~name:"Orphan"
             ~description:"" ~owner:"agent-owner" () with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok sb -> Board.Sub_board_id.to_string sb.Board.id
  in
  let post_result =
    Board_dispatch.create_post ~author:"agent-owner" ~content:"post in orphan"
      ~title:"Orphan post" ~hearth:"orphan-hearth" ~post_kind:Board.Human_post ()
  in
  let post_id =
    match post_result with
    | Error e -> Alcotest.fail (Board.show_board_error e)
    | Ok post -> Board.Post_id.to_string post.id
  in
  (match Board_dispatch.get_post ~post_id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok post ->
       Alcotest.(check (option string)) "post has hearth before delete"
         (Some "orphan-hearth") post.Board.hearth);
  (match Board_dispatch.delete_sub_board ~sub_board_id:sb_id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok () -> ());
  (match Board_dispatch.get_post ~post_id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok post ->
       Alcotest.(check (option string)) "post hearth cleared after sub-board delete"
         None post.Board.hearth);
  Board_dispatch.flush ();
  Board.reset_global_for_test ();
  Board_dispatch.reset_for_test ();
  Board_dispatch.init_jsonl ();
  (match Board_dispatch.get_post ~post_id with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok post ->
       Alcotest.(check (option string)) "cleared hearth survives restart" None
         post.Board.hearth)

let test_sub_board_post_count_projection () =
  ignore
    (Board_dispatch.create_sub_board ~slug:"counted" ~name:"Counted"
       ~description:"" ~owner:"agent-owner" ());
  ignore
    (Board_dispatch.create_post ~author:"agent-a" ~content:"one"
       ~post_kind:Board.Human_post ~hearth:"counted" ());
  ignore
    (Board_dispatch.create_post ~author:"agent-b" ~content:"two"
       ~post_kind:Board.Human_post ~hearth:"COUNTED" ());
  ignore
    (Board_dispatch.create_post ~author:"agent-c" ~content:"other"
       ~post_kind:Board.Human_post ~hearth:"other" ());
  (match Board_dispatch.get_sub_board ~sub_board_id:"counted" with
   | Error e -> Alcotest.fail (Board.show_board_error e)
   | Ok sb -> Alcotest.(check int) "derived post_count" 2 sb.Board.post_count);
  let listed =
    Board_dispatch.list_sub_boards ()
    |> List.find_opt (fun sb -> String.equal sb.Board.slug "counted")
  in
  match listed with
  | None -> Alcotest.fail "counted sub-board missing from list"
  | Some sb -> Alcotest.(check int) "listed post_count" 2 sb.Board.post_count

(** {1 Test Runner} *)

let () =
  Alcotest.run "Board_dispatch" [
    "backend", [
      Alcotest.test_case "default backend" `Quick (with_eio test_default_backend);
      Alcotest.test_case "returns jsonl" `Quick (with_eio test_backend_returns_jsonl);
    ];
    "posts", [
      Alcotest.test_case "create and get" `Quick (with_eio test_create_and_get_post);
      Alcotest.test_case "update by owner persists" `Quick
        (with_eio test_update_post_by_owner);
      Alcotest.test_case "update rejects non-owner" `Quick
        (with_eio test_update_post_rejects_non_owner);
      Alcotest.test_case "update transfers author by owner" `Quick
        (with_eio test_update_post_transfers_author_by_owner);
      Alcotest.test_case "update rejects non-owner transfer" `Quick
        (with_eio test_update_post_rejects_non_owner_transfer);
      Alcotest.test_case "update rejects invalid new_author" `Quick
        (with_eio test_update_post_rejects_invalid_new_author);
      Alcotest.test_case "update missing id" `Quick
        (with_eio test_update_post_missing_id);
      Alcotest.test_case "update rejects empty content" `Quick
        (with_eio test_update_post_rejects_empty_content);
      Alcotest.test_case "update with explicit title and body" `Quick
        (with_eio test_update_post_with_explicit_title_and_body);
      Alcotest.test_case "update preserves metadata and body" `Quick
        (with_eio test_update_post_preserves_meta_and_body);
      Alcotest.test_case "keeper hook failure does not abort create" `Quick
        (with_eio test_keeper_signal_hook_failure_does_not_abort_create_post);
      Alcotest.test_case "keeper hook cancellation propagates" `Quick
        (with_eio test_keeper_signal_hook_cancellation_propagates);
      Alcotest.test_case "identical content creates two posts" `Quick
        (with_eio test_identical_content_creates_two_posts);
      Alcotest.test_case "create append failure returns error without fanout" `Quick
        (with_eio test_create_post_persistence_failure_returns_error_without_fanout);
      Alcotest.test_case "structured roundtrip" `Quick (with_eio test_structured_post_roundtrip);
      Alcotest.test_case "SSE post_created includes post_kind" `Quick
        (with_eio test_board_sse_post_created_includes_post_kind);
      Alcotest.test_case "list" `Quick (with_eio test_list_posts);
      Alcotest.test_case "negative list limit" `Quick
        (with_eio test_list_posts_negative_limit_returns_empty);
      Alcotest.test_case "sort orders" `Quick (with_eio test_list_posts_with_sort);
      Alcotest.test_case
        "first observation starts at current head"
        `Quick
        (with_eio test_first_board_observation_starts_at_current_head);
      Alcotest.test_case
        "dashboard projection does not produce attention candidate"
        `Quick
        (with_eio test_dashboard_projection_does_not_produce_attention_candidate);
      Alcotest.test_case "recent bypasses hot cutoff" `Quick
        (with_eio test_recent_sort_bypasses_hot_cutoff);
      Alcotest.test_case "filters" `Quick (with_eio test_list_posts_with_filters);
      Alcotest.test_case "comment author filter" `Quick
        (with_eio test_list_posts_matches_comment_author);
      Alcotest.test_case "literal wildcard filter" `Quick
        (with_eio test_author_filter_treats_wildcards_literally);
      Alcotest.test_case "author match precedes recent result limit" `Quick
        (with_eio test_recent_author_match_applies_before_limit);
      Alcotest.test_case "post without kind is rejected" `Quick
        (with_eio test_post_without_kind_is_rejected);
    ];
    "comments", [
      Alcotest.test_case "add and get" `Quick (with_eio test_add_and_get_comments);
      Alcotest.test_case "comment persists post reply_count" `Quick
        (with_eio test_comment_persists_post_reply_count);
      Alcotest.test_case "comment append failure rolls back without fanout" `Quick
        (with_eio test_add_comment_persistence_failure_rolls_back_without_fanout);
      Alcotest.test_case "get_post_and_comments atomic" `Quick
        (with_eio test_get_post_and_comments_atomic);
      Alcotest.test_case "get_post_and_comments pagination clamps" `Quick
        (with_eio test_get_post_and_comments_pagination_clamps);
      Alcotest.test_case "get_post_and_comments missing post" `Quick
        (with_eio test_get_post_and_comments_missing_post);
    ];
    "votes", [
      Alcotest.test_case "upvote" `Quick (with_eio test_vote_post);
      Alcotest.test_case "dedup" `Quick (with_eio test_vote_dedup);
      Alcotest.test_case "flip" `Quick (with_eio test_vote_flip);
      Alcotest.test_case "current vote lookup" `Quick
        (with_eio test_current_vote_lookup);
      Alcotest.test_case "vote persisted by flusher actor" `Quick
        test_vote_persisted_by_flusher_actor;
      Alcotest.test_case "flusher start retries forced CAS conflicts" `Quick
        test_flusher_start_retries_forced_cas_conflicts;
      Alcotest.test_case "flusher start backoff doubles and caps" `Quick
        test_flusher_start_backoff_delay_doubles_and_caps;
    ];
    "reactions", [
      Alcotest.test_case "toggle and summary" `Quick
        (with_eio test_reaction_toggle_and_summary);
      Alcotest.test_case "persistence failure returns error without publish" `Quick
        (with_eio test_reaction_persistence_failure_returns_error_without_publish);
      Alcotest.test_case "comment reaction survives restart" `Quick
        (with_eio test_comment_reaction_survives_restart);
      Alcotest.test_case "summary recent user ids" `Quick
        (with_eio test_reaction_summary_recent_user_ids);
      Alcotest.test_case "summary batch" `Quick
        (with_eio test_reaction_summary_batch);
       Alcotest.test_case "dashboard detail binds reaction actor" `Quick
         (with_eio test_dashboard_detail_uses_authenticated_reaction_actor);
       Alcotest.test_case "post detail response format boundary" `Quick
         test_board_post_response_format_query_contract;
       Alcotest.test_case "SSE reaction_changed" `Quick
        (with_eio test_board_sse_reaction_changed);
      Alcotest.test_case "board signal reaction_changed resolves comment parent" `Quick
        (with_eio test_board_signal_reaction_changed_resolves_comment_parent);
      Alcotest.test_case "board signal vote_cast on post" `Quick
        (with_eio test_board_signal_vote_cast_on_post);
      Alcotest.test_case "board signal vote_cast on comment names comment author" `Quick
        (with_eio test_board_signal_vote_cast_on_comment_names_comment_author);
      Alcotest.test_case "unsupported emoji rejected" `Quick
        (with_eio test_reaction_rejects_unsupported_emoji);
    ];
    "misc", [
      Alcotest.test_case "vote returns total score, not change" `Quick
        (with_eio test_vote_returns_total_score_not_change);
      Alcotest.test_case "stats" `Quick (with_eio test_stats);
      Alcotest.test_case "search" `Quick (with_eio test_search);
      Alcotest.test_case "hearths" `Quick (with_eio test_hearths);
      Alcotest.test_case "set_thread_id" `Quick (with_eio test_set_thread_id);
      Alcotest.test_case "set_pinned toggle + restart" `Quick (with_eio test_set_pinned);
      Alcotest.test_case "set_pinned missing post" `Quick (with_eio test_set_pinned_missing_post);
      Alcotest.test_case "set_pinned append failure rolls back" `Quick
        (with_eio test_set_pinned_persistence_failure_rolls_back);
    ];
    "validation", [
      Alcotest.test_case "empty content" `Quick (with_eio test_empty_content);
      Alcotest.test_case "invalid author" `Quick (with_eio test_invalid_author);
      Alcotest.test_case "Direct requires exact targets" `Quick
        (with_eio test_direct_post_requires_exact_targets);
      Alcotest.test_case "malformed target is read as prose" `Quick
        (with_eio test_malformed_target_is_read_as_prose);
      Alcotest.test_case "prose reads preserve validation diagnostics" `Quick
        (with_eio test_prose_mentions_do_not_record_validation_rejections);
      Alcotest.test_case "unlisted post is not discoverable" `Quick
        (with_eio test_unlisted_post_is_not_discoverable);
      Alcotest.test_case "write emits typed audience" `Quick
        (with_eio test_write_boundary_emits_typed_audience);
      Alcotest.test_case "target case is identity-level" `Quick
        (with_eio test_target_case_is_identity_level);
    ];
    "sub_boards", [
      Alcotest.test_case "create and get" `Quick (with_eio test_sub_board_create_and_get);
      Alcotest.test_case "list" `Quick (with_eio test_sub_board_list);
      Alcotest.test_case "slug conflict" `Quick (with_eio test_sub_board_slug_conflict);
      Alcotest.test_case "delete" `Quick (with_eio test_sub_board_delete);
      Alcotest.test_case "persisted create/delete snapshot" `Quick
        (with_eio test_sub_board_create_delete_persisted_snapshot);
      Alcotest.test_case "default access open" `Quick (with_eio test_sub_board_access_default_open);
      Alcotest.test_case "members include owner" `Quick (with_eio test_sub_board_members_include_owner);
      Alcotest.test_case "members-only post policy" `Quick (with_eio test_sub_board_members_only_post_policy);
      Alcotest.test_case "owner-only post policy" `Quick (with_eio test_sub_board_owner_only_post_policy);
      Alcotest.test_case "members-only comment policy" `Quick (with_eio test_sub_board_members_only_comment_policy);
      Alcotest.test_case "owner-only comment policy" `Quick (with_eio test_sub_board_owner_only_comment_policy);
      Alcotest.test_case "open sub-board allows non-member comment" `Quick (with_eio test_sub_board_open_comment_policy_allows_anyone);
      Alcotest.test_case "derived post count" `Quick (with_eio test_sub_board_post_count_projection);
      Alcotest.test_case "update" `Quick (with_eio test_sub_board_update);
      Alcotest.test_case "delete clears orphan hearth" `Quick (with_eio test_sub_board_delete_clears_orphan_hearth);
    ];
  ]
