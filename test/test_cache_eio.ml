(** Test Cache_eio Module - Pure Synchronous Tests *)

open Masc_mcp

let () = Random.self_init ()

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let with_temp_masc_dir f =
  let base =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "masc-cache-eio-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
  in
  Unix.mkdir base 0o755;
  let config = Room.default_config base in
  let _ = Room.init config ~agent_name:None in
  try
    let result = f config in
    let _ = Room.reset config in
    rm_rf base;
    result
  with e ->
    let _ = Room.reset config in
    rm_rf base;
    raise e

let test_set_and_get () =
  with_temp_masc_dir (fun config ->
    (* Set a value - pure sync *)
    let result = Cache_eio.set config ~key:"test-key" ~value:"test-value" () in
    assert (Result.is_ok result);

    (* Get the value - pure sync *)
    match Cache_eio.get config ~key:"test-key" with
    | Ok (Some entry) ->
      assert (entry.Cache_eio.key = "test-key");
      assert (entry.Cache_eio.value = "test-value")
    | _ -> failwith "Expected to get cached value"
  );
  print_endline "✓ test_set_and_get passed"

let test_set_with_ttl () =
  with_temp_masc_dir (fun config ->
    (* Set with TTL *)
    let result = Cache_eio.set config ~key:"ttl-key" ~value:"ttl-value"
                   ~ttl_seconds:3600 () in
    assert (Result.is_ok result);

    match Cache_eio.get config ~key:"ttl-key" with
    | Ok (Some entry) ->
      assert (entry.Cache_eio.key = "ttl-key");
      assert (Option.is_some entry.Cache_eio.expires_at)
    | _ -> failwith "Expected to get cached value with TTL"
  );
  print_endline "✓ test_set_with_ttl passed"

let test_set_with_tags () =
  with_temp_masc_dir (fun config ->
    (* Set with tags *)
    let result = Cache_eio.set config ~key:"tagged-key" ~value:"tagged-value"
                   ~tags:["tag1"; "tag2"] () in
    assert (Result.is_ok result);

    match Cache_eio.get config ~key:"tagged-key" with
    | Ok (Some entry) ->
      assert (List.mem "tag1" entry.Cache_eio.tags);
      assert (List.mem "tag2" entry.Cache_eio.tags)
    | _ -> failwith "Expected to get cached value with tags"
  );
  print_endline "✓ test_set_with_tags passed"

let test_get_nonexistent () =
  with_temp_masc_dir (fun config ->
    match Cache_eio.get config ~key:"nonexistent" with
    | Ok None -> ()  (* Expected *)
    | _ -> failwith "Expected None for nonexistent key"
  );
  print_endline "✓ test_get_nonexistent passed"

let test_delete () =
  with_temp_masc_dir (fun config ->
    (* Set then delete *)
    let _ = Cache_eio.set config ~key:"delete-me" ~value:"value" () in

    let deleted = Cache_eio.delete config ~key:"delete-me" in
    assert (Result.is_ok deleted);
    assert (deleted = Ok true);

    (* Verify it's gone *)
    match Cache_eio.get config ~key:"delete-me" with
    | Ok None -> ()
    | _ -> failwith "Expected key to be deleted"
  );
  print_endline "✓ test_delete passed"

let test_list () =
  with_temp_masc_dir (fun config ->
    (* Add some entries *)
    let _ = Cache_eio.set config ~key:"list-1" ~value:"v1" ~tags:["a"] () in
    let _ = Cache_eio.set config ~key:"list-2" ~value:"v2" ~tags:["a"; "b"] () in
    let _ = Cache_eio.set config ~key:"list-3" ~value:"v3" ~tags:["b"] () in

    (* List all *)
    let all = Cache_eio.list config () in
    assert (List.length all = 3);

    (* List by tag *)
    let tag_a = Cache_eio.list config ~tag:"a" () in
    assert (List.length tag_a = 2);

    let tag_b = Cache_eio.list config ~tag:"b" () in
    assert (List.length tag_b = 2)
  );
  print_endline "✓ test_list passed"

let test_clear () =
  with_temp_masc_dir (fun config ->
    (* Add some entries *)
    let _ = Cache_eio.set config ~key:"clear-1" ~value:"v1" () in
    let _ = Cache_eio.set config ~key:"clear-2" ~value:"v2" () in

    (* Clear all *)
    let result = Cache_eio.clear config in
    assert (Result.is_ok result);
    (match result with
     | Ok count -> assert (count = 2)
     | Error _ -> failwith "Clear failed");

    (* Verify empty *)
    let all = Cache_eio.list config () in
    assert (List.length all = 0)
  );
  print_endline "✓ test_clear passed"

let test_stats () =
  with_temp_masc_dir (fun config ->
    (* Add some entries *)
    let _ = Cache_eio.set config ~key:"stats-1" ~value:"short" () in
    let _ = Cache_eio.set config ~key:"stats-2" ~value:(String.make 1000 'x') () in

    let result = Cache_eio.stats config in
    match result with
    | Ok (total, expired, size) ->
      assert (total = 2);
      assert (expired = 0);
      assert (size > 0.0);
      print_endline (Printf.sprintf "  Stats: %s" (Cache_eio.format_stats (total, expired, size)))
    | Error _ -> failwith "Stats failed"
  );
  print_endline "✓ test_stats passed"

let test_expired_cleanup () =
  with_temp_masc_dir (fun config ->
    (* Set with very short TTL (already expired) *)
    let _ = Cache_eio.set config ~key:"expired" ~value:"old" ~ttl_seconds:(-1) () in

    (* Get should return None and delete *)
    match Cache_eio.get config ~key:"expired" with
    | Ok None -> ()  (* Auto-cleanup worked *)
    | _ -> failwith "Expected expired entry to be cleaned up"
  );
  print_endline "✓ test_expired_cleanup passed"

let () =
  print_endline "\n=== Cache_eio Tests (Pure Sync) ===\n";
  test_set_and_get ();
  test_set_with_ttl ();
  test_set_with_tags ();
  test_get_nonexistent ();
  test_delete ();
  test_list ();
  test_clear ();
  test_stats ();
  test_expired_cleanup ();
  print_endline "\n✅ All 9 Cache_eio tests passed!\n"
