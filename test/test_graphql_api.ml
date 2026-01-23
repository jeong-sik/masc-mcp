(** GraphQL API tests (read-only queries). *)

module Graphql_api = Masc_mcp.Graphql_api
module Room = Masc_mcp.Room
module Room_utils = Masc_mcp.Room_utils

let temp_dir () =
  let dir = Filename.temp_file "test_graphql_api_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  dir

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path
  in
  try rm dir with _ -> ()

let graphql_query config query =
  let body = Yojson.Safe.to_string (`Assoc [("query", `String query)]) in
  let response = Graphql_api.handle_request ~config body in
  (match response.status with
   | `OK -> ()
   | `Bad_request -> Alcotest.fail "GraphQL response status is bad_request");
  Yojson.Safe.from_string response.body

let test_status_query () =
  let base_path = temp_dir () in
  let config = Room_utils.default_config base_path in
  let _ = Room.init config ~agent_name:None in
  let json = graphql_query config "{ status { project paused } }" in
  let open Yojson.Safe.Util in
  let project = json |> member "data" |> member "status" |> member "project" |> to_string in
  let paused = json |> member "data" |> member "status" |> member "paused" |> to_bool in
  Alcotest.(check string) "project" (Filename.basename base_path) project;
  Alcotest.(check bool) "paused" false paused;
  cleanup_dir base_path

let test_tasks_connection () =
  let base_path = temp_dir () in
  let config = Room_utils.default_config base_path in
  let _ = Room.init config ~agent_name:None in
  let _ = Room.add_task config ~title:"GraphQL task" ~priority:2 ~description:"test" in
  let json =
    graphql_query config
      "{ tasks(first: 10) { totalCount edges { node { title } } } }"
  in
  let open Yojson.Safe.Util in
  let total = json |> member "data" |> member "tasks" |> member "totalCount" |> to_int in
  let edges = json |> member "data" |> member "tasks" |> member "edges" |> to_list in
  let title =
    match edges with
    | first :: _ -> first |> member "node" |> member "title" |> to_string
    | [] -> ""
  in
  Alcotest.(check int) "totalCount" 1 total;
  Alcotest.(check string) "title" "GraphQL task" title;
  cleanup_dir base_path

let () =
  let open Alcotest in
  run "Graphql_api"
    [
      ("status", [test_case "status query" `Quick test_status_query]);
      ("tasks", [test_case "tasks connection" `Quick test_tasks_connection]);
    ]
