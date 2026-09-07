(** test_fs_compat.ml - Fs_compat safety + behavior tests

    Focus: ensure mkdir_p fallback does not invoke a shell and correctly creates
    nested directories with tricky path characters.
*)

open Alcotest

let with_tmp_dir (f : string -> unit) : unit =
  let tmp = Filename.temp_file "masc_fs_compat_" ".tmp" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  Fun.protect ~finally:(fun () -> Fs_compat.remove_tree tmp) (fun () -> f tmp)
;;

let rec mkdir_p_unix path =
  if path = "" || path = Filename.dirname path then ()
  else (
    mkdir_p_unix (Filename.dirname path);
    if not (Sys.file_exists path) then Unix.mkdir path 0o755)
;;

let test_mkdir_p_creates_nested_dirs () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "a/b/c" in
  Fs_compat.mkdir_p path;
  check bool "created leaf dir" true (Sys.file_exists path && Sys.is_directory path)
;;

let test_mkdir_p_does_not_shell_inject () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let pwned = Filename.concat base "pwned" in
  let dangerous = Filename.concat base ("dir;touch " ^ pwned ^ "/x") in
  Fs_compat.mkdir_p dangerous;
  (* If mkdir_p used a shell (`mkdir -p <path>`), this would create pwned. *)
  check bool "no shell side-effect file created" false (Sys.file_exists pwned);
  check
    bool
    "dangerous path exists"
    true
    (Sys.file_exists dangerous && Sys.is_directory dangerous)
;;

let test_remove_tree_does_not_shell_inject () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let marker = Filename.concat base "pwned" in
  let dangerous = Filename.concat base ("dir;touch " ^ marker) in
  mkdir_p_unix (Filename.dirname dangerous);
  Unix.mkdir dangerous 0o755;
  Fs_compat.remove_tree dangerous;
  check bool "target removed" false (Sys.file_exists dangerous);
  (* If remove_tree used a shell (`rm -rf <path>`), this would create marker. *)
  check bool "no shell side-effect file created" false (Sys.file_exists marker)
;;

let test_remove_tree_unlinks_symlink_without_following () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "target" in
  let link = Filename.concat base "link" in
  Unix.mkdir target 0o755;
  Fs_compat.save_file (Filename.concat target "kept.txt") "kept";
  Unix.symlink target link;
  Fs_compat.remove_tree link;
  check bool "symlink removed" false (Sys.file_exists link);
  check bool "target directory preserved" true (Sys.file_exists target);
  check string "target content preserved" "kept" (Fs_compat.load_file (Filename.concat target "kept.txt"))
;;

let test_save_file_atomic_leaves_no_tmp_on_success () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  (match Fs_compat.save_file_atomic target {|{"ok":true}|} with
   | Ok () -> ()
   | Error msg -> fail msg);
  check bool "target exists" true (Sys.file_exists target);
  check string "content matches" {|{"ok":true}|} (Fs_compat.load_file target);
  let leftover_tmps =
    Sys.readdir base
    |> Array.to_list
    |> List.filter (fun n -> String.length n >= 8 && String.sub n 0 8 = ".atomic_")
  in
  check (list string) "no leftover .atomic_ tmp files" [] leftover_tmps
;;

let test_open_atomic_temp_file_uses_canonical_shape () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let path, channel = Fs_compat.open_atomic_temp_file ~temp_dir:base () in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr channel;
      if Sys.file_exists path then Sys.remove path)
    (fun () ->
       let name = Filename.basename path in
       check string "temp file is created in requested directory" base
         (Filename.dirname path);
       check bool "canonical prefix" true
         (String.starts_with name ~prefix:".atomic_");
       check bool "shared orphan matcher recognizes writer output" true
         (Fs_compat.is_atomic_orphan_name name);
       output_string channel "payload";
       close_out channel;
       check string "returned channel writes the temp file" "payload"
         (Fs_compat.load_file path))
;;

let test_open_atomic_temp_file_writes_bytes_verbatim () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  (* CR, LF, NUL, and 0x1a are the bytes a text-mode channel is allowed to
     rewrite. The durable path promises the caller's exact bytes, so the
     temp channel is opened binary. *)
  let payload = "a\r\nb\n\000c\026d" in
  let path, channel = Fs_compat.open_atomic_temp_file ~temp_dir:base () in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr channel;
      if Sys.file_exists path then Sys.remove path)
    (fun () ->
       output_string channel payload;
       close_out channel;
       let read_back = Fs_compat.load_file path in
       check int "byte length survives the round trip" (String.length payload)
         (String.length read_back);
       check string "bytes survive the round trip" payload read_back)
;;

let test_save_file_atomic_overwrites_existing () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  Fs_compat.save_file target "old";
  (match Fs_compat.save_file_atomic target "new" with
   | Ok () -> ()
   | Error msg -> fail msg);
  check string "overwrite succeeded" "new" (Fs_compat.load_file target)
;;

let test_save_file_atomic_returns_temp_creation_failure () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  (* Force temp-file creation to fail without relying on DAC write-blocking.
     The target's parent directory does not exist, so Filename.temp_file fails
     with ENOENT (a path-resolution error, not a permission check) for every
     UID, root included. The previous approach used Unix.chmod blocked 0o500,
     but chmod is a no-op for root (UID 0): under a root sandbox temp_file
     succeeded and the regression assertion could not run. See masc#25210. *)
  let missing = Filename.concat base "does_not_exist" in
  let target = Filename.concat missing "out.json" in
  let result = Fs_compat.save_file_atomic target "new" in
  match result with
  | Error message ->
    check bool "temp creation error names the atomic operation" true
      (String.starts_with message ~prefix:"save_file_atomic ")
  | Ok () -> fail "atomic save unexpectedly wrote into a missing directory"
;;

let test_save_file_atomic_strict_rejects_unsupported_payload_sync () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  let parent_sync_called = ref false in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.Atomic_replace_for_testing.save_file_atomic_strict_staged
       ~sync_file:(fun path ->
         raise (Unix.Unix_error (Unix.EOPNOTSUPP, "fsync", path)))
       ~sync_parent:(fun _ -> parent_sync_called := true)
       target
       "new"
   with
   | Error ({ stage = Fs_compat.Before_rename; _ } as failure) ->
     (match failure.exception_ with
      | Unix.Unix_error (Unix.EOPNOTSUPP, _, _) -> ()
      | exception_ ->
        failf "unexpected payload sync failure: %s" (Printexc.to_string exception_))
   | Error failure ->
     failf
       "unsupported payload sync reported after rename: %s"
       (Fs_compat.atomic_replace_failure_to_string failure)
   | Ok () -> fail "unsupported payload sync was accepted");
  check bool "parent sync not reached" false !parent_sync_called;
  check string "target remains old" "old" (Fs_compat.load_file target)
;;

let test_save_file_atomic_strict_payload_sync_cancellation () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  let parent_sync_called = ref false in
  let cancellation = Eio.Cancel.Cancelled Exit in
  let cancellation_backtrace = Printexc.get_callstack 32 in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.Atomic_replace_for_testing.save_file_atomic_strict_staged
       ~sync_file:(fun _ ->
         Printexc.raise_with_backtrace cancellation cancellation_backtrace)
       ~sync_parent:(fun _ -> parent_sync_called := true)
       target
       "new"
   with
   | Error ({ stage = Fs_compat.Before_rename; _ } as failure) ->
     check bool "original cancellation preserved" true
       (failure.exception_ == cancellation);
     check string "original cancellation backtrace preserved"
       (Printexc.raw_backtrace_to_string cancellation_backtrace)
       (Printexc.raw_backtrace_to_string failure.backtrace)
   | Error failure ->
     failf
       "payload sync cancellation reported after rename: %s"
       (Fs_compat.atomic_replace_failure_to_string failure)
   | Ok () -> fail "payload sync cancellation was accepted");
  check bool "parent sync not reached" false !parent_sync_called;
  check string "target remains old" "old" (Fs_compat.load_file target);
  let leftover_tmps =
    Sys.readdir base
    |> Array.to_list
    |> List.filter Fs_compat.is_atomic_orphan_name
  in
  check (list string) "payload cancellation leaves no atomic temp" [] leftover_tmps
;;

let test_save_file_atomic_commit_runs_off_the_fiber () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  let contexts = ref [] in
  let record _ = contexts := Fs_compat.execution_context () :: !contexts in
  check bool "the test itself runs in an Eio fiber" true
    (Fs_compat.execution_context () = Fs_compat.Eio_fiber);
  (match
     Fs_compat.Atomic_replace_for_testing.save_file_atomic_strict_staged
       ~sync_file:record
       ~sync_parent:record
       target
       "new"
   with
   | Ok () -> ()
   | Error failure ->
     failf "atomic save failed: %s" (Fs_compat.atomic_replace_failure_to_string failure));
  check int "payload and parent sync both ran" 2 (List.length !contexts);
  check bool "neither sync ran on the calling fiber" true
    (List.for_all (fun context -> context = Fs_compat.Non_eio) !contexts);
  check string "target holds the new content" "new" (Fs_compat.load_file target)
;;

let check_save_file_atomic_strict_parent_sync_failure
      ~label
      ~exception_
  =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "out.json" in
  let injected_backtrace = Printexc.get_callstack 32 in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.Atomic_replace_for_testing.save_file_atomic_strict_staged
       ~sync_file:(fun _ -> ())
       ~sync_parent:(fun _ ->
         Printexc.raise_with_backtrace exception_ injected_backtrace)
       target
       "new"
   with
   | Error ({ stage = Fs_compat.After_rename; _ } as failure) ->
     check bool (label ^ " preserves original exception") true
       (failure.exception_ == exception_);
     check string (label ^ " preserves original backtrace")
       (Printexc.raw_backtrace_to_string injected_backtrace)
       (Printexc.raw_backtrace_to_string failure.backtrace)
   | Error failure ->
     failf
       "%s reported before rename: %s"
       label
       (Fs_compat.atomic_replace_failure_to_string failure)
   | Ok () -> failf "%s was accepted" label);
  check string (label ^ " leaves new target visible") "new"
    (Fs_compat.load_file target)
;;

let test_save_file_atomic_strict_parent_sync_error () =
  check_save_file_atomic_strict_parent_sync_failure
    ~label:"parent sync error"
    ~exception_:(Unix.Unix_error (Unix.EIO, "fsync", "parent"))
;;

let test_save_file_atomic_strict_parent_sync_cancellation () =
  check_save_file_atomic_strict_parent_sync_failure
    ~label:"parent sync cancellation"
    ~exception_:(Eio.Cancel.Cancelled Exit)
;;

let test_streaming_atomic_replace_preserves_bytes () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "runs.jsonl" in
  let chunks = [ "first\r\n"; "\000second\026"; "\nthird\n" ] in
  let callback_context = ref None in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.write_file_atomic_strict_staged target ~write:(fun channel ->
       callback_context := Some (Fs_compat.execution_context ());
       List.iter (output_string channel) chunks;
       flush channel;
       check string "staged bytes have not replaced the target" "old"
         (Fs_compat.load_file target))
   with
   | Ok () -> ()
   | Error failure -> fail (Fs_compat.atomic_replace_failure_to_string failure));
  check bool "stream callback runs off the Eio fiber" true
    (!callback_context = Some Fs_compat.Non_eio);
  check string "every streamed byte is published" (String.concat "" chunks)
    (Fs_compat.load_file target);
  check (list string) "successful replacement removes its stage" [ "runs.jsonl" ]
    (Sys.readdir base |> Array.to_list)
;;

let check_streaming_atomic_callback_failure ~exception_ () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "runs.jsonl" in
  let injected_backtrace = Printexc.get_callstack 32 in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.write_file_atomic_strict_staged target ~write:(fun channel ->
       output_string channel "partial row";
       flush channel;
       Printexc.raise_with_backtrace exception_ injected_backtrace)
   with
   | Error ({ stage = Fs_compat.Before_rename; _ } as failure) ->
     check bool "callback exception is preserved" true
       (failure.exception_ == exception_);
     check string "callback backtrace is preserved"
       (Printexc.raw_backtrace_to_string injected_backtrace)
       (Printexc.raw_backtrace_to_string failure.backtrace)
   | Error failure -> fail (Fs_compat.atomic_replace_failure_to_string failure)
   | Ok () -> fail "failed streaming callback was published");
  check string "failed stream preserves original bytes" "old"
    (Fs_compat.load_file target);
  check (list string) "partial stage is removed" [ "runs.jsonl" ]
    (Sys.readdir base |> Array.to_list)
;;

let test_streaming_atomic_parent_sync_failure () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let target = Filename.concat base "runs.jsonl" in
  let exception_ = Unix.Unix_error (Unix.EIO, "fsync", base) in
  let injected_backtrace = Printexc.get_callstack 32 in
  Fs_compat.save_file target "old";
  (match
     Fs_compat.Atomic_replace_for_testing.write_file_atomic_strict_staged
       ~sync_parent:(fun _ ->
         Printexc.raise_with_backtrace exception_ injected_backtrace)
       target
       ~write:(fun channel ->
         output_string channel "first\n";
         output_string channel "second\n")
   with
   | Error ({ stage = Fs_compat.After_rename; _ } as failure) ->
     check bool "parent sync exception is preserved" true
       (failure.exception_ == exception_);
     check string "parent sync backtrace is preserved"
       (Printexc.raw_backtrace_to_string injected_backtrace)
       (Printexc.raw_backtrace_to_string failure.backtrace)
   | Error failure -> fail (Fs_compat.atomic_replace_failure_to_string failure)
   | Ok () -> fail "failed parent sync was accepted");
  check string "complete replacement remains visible after rename" "first\nsecond\n"
    (Fs_compat.load_file target);
  check (list string) "published stream leaves no temporary file" [ "runs.jsonl" ]
    (Sys.readdir base |> Array.to_list)
;;

(* Inside Eio the three primitives run their Unix implementation on a
   system thread; the bytes, the append order and the file mode must be the
   ones the inline implementation produces. *)
let test_primitives_agree_inside_and_outside_eio ~fs () =
  with_tmp_dir
  @@ fun base ->
  let run implementation =
    let target = Filename.concat base (implementation ^ ".txt") in
    Fs_compat.save_file target "first\n";
    Fs_compat.append_file target "second\n";
    Fs_compat.append_file target "third\n";
    let contents = Fs_compat.load_file target in
    let mode = (Unix.stat target).Unix.st_perm in
    contents, mode
  in
  Fs_compat.clear_fs ();
  let inline = run "inline" in
  Fs_compat.set_fs fs;
  let eio = Fun.protect ~finally:Fs_compat.clear_fs (fun () -> run "eio") in
  check string "same bytes" "first\nsecond\nthird\n" (fst inline);
  check string "eio path writes the same bytes" (fst inline) (fst eio);
  check int "eio path creates the same mode" (snd inline) (snd eio)
;;

let test_read_dir_and_path_kind_use_typed_inventory ~fs () =
  with_tmp_dir
  @@ fun base ->
  let directory = Filename.concat base "inventory" in
  let regular = Filename.concat directory "b.json" in
  let nested = Filename.concat directory "a-dir" in
  let nested_link = Filename.concat directory "c-link" in
  let fifo = Filename.concat directory "d-fifo" in
  Fs_compat.mkdir_p directory;
  Fs_compat.mkdir_p nested;
  Fs_compat.save_file regular "{}";
  Unix.symlink nested nested_link;
  Unix.mkfifo fifo 0o600;
  let check_inventory implementation =
    check bool (implementation ^ " directory classified") true
      (Fs_compat.path_kind directory = Fs_compat.Directory);
    check bool (implementation ^ " regular file classified") true
      (Fs_compat.path_kind regular = Fs_compat.Other);
    check bool (implementation ^ " missing path classified") true
      (Fs_compat.path_kind (Filename.concat directory "missing") = Fs_compat.Missing);
    check bool (implementation ^ " symlink target followed") true
      (Fs_compat.path_kind nested_link = Fs_compat.Directory);
    check bool (implementation ^ " symlink itself remains non-directory") true
      (Fs_compat.path_kind ~follow:false nested_link = Fs_compat.Other);
    check bool (implementation ^ " exact regular file kind") true
      (Fs_compat.exact_path_kind ~follow:false regular
       = Fs_compat.Exact_kind Unix.S_REG);
    check bool (implementation ^ " exact symlink kind") true
      (Fs_compat.exact_path_kind ~follow:false nested_link
       = Fs_compat.Exact_kind Unix.S_LNK);
    check bool (implementation ^ " exact FIFO kind") true
      (Fs_compat.exact_path_kind ~follow:false fifo
       = Fs_compat.Exact_kind Unix.S_FIFO);
    check bool (implementation ^ " exact missing kind") true
      (Fs_compat.exact_path_kind (Filename.concat directory "missing")
       = Fs_compat.Exact_missing);
    check
      (list string)
      (implementation ^ " directory inventory is deterministic")
      [ "a-dir"; "b.json"; "c-link"; "d-fifo" ]
      (Fs_compat.read_dir directory)
  in
  Fs_compat.clear_fs ();
  check bool "fallback blocked descendant kind stays typed" true
    (Fs_compat.exact_path_kind (Filename.concat regular "blocked-child")
     = Fs_compat.Exact_unknown);
  check bool "fallback blocked descendant is not missing" true
    (Fs_compat.path_kind (Filename.concat regular "blocked-child")
     = Fs_compat.Other);
  check_inventory "fallback";
  Fs_compat.set_fs fs;
  check_inventory "Eio"
;;

let test_owned_regular_file_read_uses_eio_systhread ~fs () =
  Fs_compat.set_fs fs;
  with_tmp_dir
  @@ fun base ->
  let parent = Filename.concat base "owned" in
  let path = Filename.concat parent "record.json" in
  Fs_compat.mkdir_p parent;
  Fs_compat.save_file path "owned";
  (match Fs_compat.load_owned_regular_file ~ownership_root:base path with
   | Ok (Some content) -> check string "descriptor content" "owned" content
   | Ok None -> fail "owned file was reported absent"
   | Error error ->
     fail (Fs_compat.owned_regular_file_read_error_to_string error));
  let cancellation_propagated =
    try
      Eio.Cancel.sub (fun context ->
        Eio.Cancel.cancel context Exit;
        ignore
          (Fs_compat.load_owned_regular_file ~ownership_root:base path
           : (string option, Fs_compat.owned_regular_file_read_error) result));
      false
    with
    | Eio.Cancel.Cancelled _ -> true
  in
  check bool "cancellation is not converted to a read error" true
    cancellation_propagated
;;

let with_redirected_stderr (f : unit -> 'a) : 'a * string =
  let tmp = Filename.temp_file "masc_stderr_" ".log" in
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stderr;
        Unix.dup2 saved Unix.stderr;
        Unix.close saved)
      f
  in
  let captured =
    let ic = open_in tmp in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    Sys.remove tmp;
    s
  in
  result, captured
;;

let test_load_jsonl_diagnostics_counts_malformed_lines () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "broken.jsonl" in
  Fs_compat.save_file path "{\"ok\":1}\nnot-json\n{\"ok\":2}\n";
  let (parsed, malformed), _ =
    with_redirected_stderr (fun () -> Fs_compat.load_jsonl_diagnostics path)
  in
  check int "parsed count" 2 (List.length parsed);
  check int "malformed count" 1 malformed
;;

let test_load_jsonl_diagnostics_warn_includes_full_path () =
  Fs_compat.clear_fs ();
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "broken.jsonl" in
  Fs_compat.save_file path "garbage\n";
  let _, captured =
    with_redirected_stderr (fun () -> Fs_compat.load_jsonl_diagnostics path)
  in
  let contains hay needle =
    let nlen = String.length needle in
    let hlen = String.length hay in
    let rec loop i =
      if i + nlen > hlen
      then false
      else if String.sub hay i nlen = needle
      then true
      else loop (i + 1)
    in
    loop 0
  in
  check
    bool
    "warn message contains full path (not just basename)"
    true
    (contains captured path)
;;

let write_lines path lines =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       List.iter
         (fun l ->
            output_string oc l;
            output_char oc '\n')
         lines)
;;

let test_fold_jsonl_lines_small () =
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "small.jsonl" in
  write_lines path [ {|{"n":1}|}; {|{"n":2}|}; {|{"n":3}|} ];
  let total =
    Fs_compat.fold_jsonl_lines
      ~init:0
      ~f:(fun acc ~line_no:_ json ->
        match json with
        | `Assoc [ (_, `Int n) ] -> acc + n
        | _ -> acc)
      path
  in
  check int "fold sum" 6 total
;;

let test_fold_jsonl_lines_skips_blank_and_malformed () =
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "mixed.jsonl" in
  write_lines path [ {|{"n":1}|}; ""; {|not-json|}; {|{"n":2}|} ];
  let collected = ref [] in
  let (), _stderr =
    with_redirected_stderr (fun () ->
      Fs_compat.fold_jsonl_lines
        ~init:()
        ~f:(fun () ~line_no:_ json -> collected := json :: !collected)
        path)
  in
  let values = List.rev !collected in
  let ns =
    List.map
      (fun json ->
         match json with
         | `Assoc [ (_, `Int n) ] -> n
         | _ -> -1)
      values
  in
  check int "valid line count (blank + malformed skipped)" 2 (List.length ns);
  check (list int) "extracted values in order" [ 1; 2 ] ns
;;

let test_fold_jsonl_lines_missing_trailing_newline () =
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "no_trailing.jsonl" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       output_string oc {|{"n":1}|};
       output_char oc '\n';
       output_string oc {|{"n":2}|});
  let total =
    Fs_compat.fold_jsonl_lines
      ~init:0
      ~f:(fun acc ~line_no:_ json ->
        match json with
        | `Assoc [ (_, `Int n) ] -> acc + n
        | _ -> acc)
      path
  in
  check int "no-trailing-newline reads last line" 3 total
;;

let test_fold_jsonl_lines_streaming_memory () =
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "big.jsonl" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       for i = 1 to 10_000 do
         output_string oc (Printf.sprintf "{\"n\":%d}\n" i)
       done);
  Gc.compact ();
  let total =
    Fs_compat.fold_jsonl_lines
      ~init:0
      ~f:(fun acc ~line_no:_ json ->
        match json with
        | `Assoc [ (_, `Int n) ] -> acc + n
        | _ -> acc)
      path
  in
  Gc.compact ();
  let live_after_bytes = (Gc.stat ()).Gc.live_words * (Sys.word_size / 8) in
  check int "fold all 10k entries" 50_005_000 total;
  (* Streaming guarantee: post-fold heap stays bounded.  The 10k-line
     file is ~100 KB on disk; allow 4x slack for test harness overhead
     so this catches a regression (e.g. accidentally retaining a list
     of all 10k JSON nodes) without being flaky. *)
  let bound = 4 * 1024 * 1024 in
  check
    bool
    (Printf.sprintf "post-fold heap < 4 MB (was %d bytes)" live_after_bytes)
    true
    (live_after_bytes < bound)
;;

let test_fold_jsonl_lines_missing_file () =
  with_tmp_dir
  @@ fun base ->
  let path = Filename.concat base "does_not_exist.jsonl" in
  let result =
    Fs_compat.fold_jsonl_lines ~init:0 ~f:(fun acc ~line_no:_ _json -> acc + 1) path
  in
  (* Consistent with [load_jsonl]: missing file returns [init] silently
     rather than raising. *)
  check int "missing file returns init" 0 result
;;

let () =
  Eio_main.run
  @@ fun env ->
  Fs_compat.set_fs (Eio.Stdenv.fs env);
  Eio_guard.enable ();
  run
    "fs_compat"
    [ ( "mkdir_p"
      , [ test_case "creates nested dirs" `Quick test_mkdir_p_creates_nested_dirs
        ; test_case "no shell injection" `Quick test_mkdir_p_does_not_shell_inject
        ] )
    ; ( "remove_tree"
      , [ test_case "no shell injection" `Quick test_remove_tree_does_not_shell_inject
        ; test_case
            "unlinks symlink without following"
            `Quick
            test_remove_tree_unlinks_symlink_without_following
        ] )
    ; ( "save_file_atomic"
      , [ test_case
            "leaves no .atomic_ tmp on success"
            `Quick
            test_save_file_atomic_leaves_no_tmp_on_success
        ; test_case
            "overwrites existing target"
            `Quick
            test_save_file_atomic_overwrites_existing
        ; test_case
            "returns temp creation failure"
            `Quick
            test_save_file_atomic_returns_temp_creation_failure
        ; test_case
            "strict payload unsupported is before rename"
            `Quick
            test_save_file_atomic_strict_rejects_unsupported_payload_sync
        ; test_case
            "strict payload cancellation is before rename"
            `Quick
            test_save_file_atomic_strict_payload_sync_cancellation
        ; test_case
            "strict parent sync error is after rename"
            `Quick
            test_save_file_atomic_strict_parent_sync_error
        ; test_case
            "strict parent sync cancellation is after rename"
            `Quick
            test_save_file_atomic_strict_parent_sync_cancellation
        ; test_case
            "commit syscalls run off the fiber"
            `Quick
            test_save_file_atomic_commit_runs_off_the_fiber
        ; test_case
            "streamed replacement preserves bytes off the fiber"
            `Quick
            test_streaming_atomic_replace_preserves_bytes
        ; test_case
            "stream callback failure preserves the target and cleans its stage"
            `Quick
            (check_streaming_atomic_callback_failure
               ~exception_:(Unix.Unix_error (Unix.EIO, "write", "stage")))
        ; test_case
            "stream callback cancellation preserves the target and cleans its stage"
            `Quick
            (check_streaming_atomic_callback_failure
               ~exception_:(Eio.Cancel.Cancelled Exit))
        ; test_case
            "stream parent sync failure retains the published target"
            `Quick
            test_streaming_atomic_parent_sync_failure
        ; test_case
            "temp writer uses canonical shared shape"
            `Quick
            test_open_atomic_temp_file_uses_canonical_shape
        ; test_case
            "temp writer keeps bytes verbatim"
            `Quick
            test_open_atomic_temp_file_writes_bytes_verbatim
        ] )
    ; ( "inventory"
      , [ test_case
            "typed path kind and sorted read_dir"
            `Quick
            (test_read_dir_and_path_kind_use_typed_inventory
               ~fs:(Eio.Stdenv.fs env))
        ; test_case
            "load, save and append agree inside and outside Eio"
            `Quick
            (test_primitives_agree_inside_and_outside_eio ~fs:(Eio.Stdenv.fs env))
        ] )
    ; ( "owned regular file"
      , [ test_case
            "Eio systhread and cancellation"
            `Quick
            (test_owned_regular_file_read_uses_eio_systhread
               ~fs:(Eio.Stdenv.fs env))
        ] )
    ; ( "load_jsonl_diagnostics"
      , [ test_case
            "counts malformed lines"
            `Quick
            test_load_jsonl_diagnostics_counts_malformed_lines
        ; test_case
            "warn includes full path"
            `Quick
            test_load_jsonl_diagnostics_warn_includes_full_path
        ] )
    ; ( "fold_jsonl_lines"
      , [ test_case "small fold sum" `Quick test_fold_jsonl_lines_small
        ; test_case
            "skips blank, surfaces line_no"
            `Quick
            test_fold_jsonl_lines_skips_blank_and_malformed
        ; test_case
            "missing trailing newline"
            `Quick
            test_fold_jsonl_lines_missing_trailing_newline
        ; test_case
            "10k lines stays streaming"
            `Quick
            test_fold_jsonl_lines_streaming_memory
        ; test_case
            "missing file returns init"
            `Quick
            test_fold_jsonl_lines_missing_file
        ] )
    ]
;;
