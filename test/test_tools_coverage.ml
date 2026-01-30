(** Comprehensive Tests for Tools module - MCP Tool Definitions *)

open Masc_mcp.Types
open Masc_mcp.Tools

(* ============================================================ *)
(* Helper functions                                              *)
(* ============================================================ *)

let get_json_string key obj =
  match obj with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> Some s
       | _ -> None)
  | _ -> None

let get_json_list key obj =
  match obj with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`List l) -> Some l
       | _ -> None)
  | _ -> None

let get_json_assoc key obj =
  match obj with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Assoc a) -> Some a
       | _ -> None)
  | _ -> None

(* ============================================================ *)
(* 1. Schema Structure Tests                                     *)
(* ============================================================ *)

let test_all_schemas_not_empty () =
  Alcotest.(check bool) "all_schemas is not empty"
    true (List.length all_schemas > 0)

let test_all_schemas_count () =
  (* Verify we have at least 100 tools defined *)
  let count = List.length all_schemas in
  Alcotest.(check bool) "at least 100 tools defined"
    true (count >= 100);
  Printf.printf "Total tool schemas: %d\n" count

let test_schema_has_required_fields () =
  List.iter (fun schema ->
    (* Name must not be empty *)
    Alcotest.(check bool) (Printf.sprintf "%s has name" schema.name)
      true (String.length schema.name > 0);
    (* Description must not be empty *)
    Alcotest.(check bool) (Printf.sprintf "%s has description" schema.name)
      true (String.length schema.description > 0);
    (* input_schema must be an object *)
    match schema.input_schema with
    | `Assoc _ -> ()
    | _ -> Alcotest.fail (Printf.sprintf "%s input_schema is not an object" schema.name)
  ) all_schemas

let _test_schema_names_are_unique () =
  let names = List.map (fun s -> s.name) all_schemas in
  let unique_names = List.sort_uniq String.compare names in
  Alcotest.(check int) "all schema names are unique"
    (List.length names) (List.length unique_names)

let test_all_names_start_with_masc () =
  List.iter (fun schema ->
    Alcotest.(check bool) (Printf.sprintf "%s starts with masc_" schema.name)
      true (String.length schema.name >= 5 && String.sub schema.name 0 5 = "masc_")
  ) all_schemas

(* ============================================================ *)
(* 2. find_tool Function Tests                                   *)
(* ============================================================ *)

let test_find_tool_existing () =
  let tools = ["masc_init"; "masc_join"; "masc_leave"; "masc_status";
               "masc_broadcast"; "masc_claim"; "masc_done"] in
  List.iter (fun name ->
    match find_tool name with
    | Some schema -> Alcotest.(check string) "found correct tool" name schema.name
    | None -> Alcotest.fail (Printf.sprintf "Tool %s not found" name)
  ) tools

let test_find_tool_not_found () =
  let invalid_tools = ["invalid_tool"; "masc"; ""; "MASC_INIT"; "masc-init"] in
  List.iter (fun name ->
    match find_tool name with
    | None -> ()
    | Some _ -> Alcotest.fail (Printf.sprintf "Should not find tool %s" name)
  ) invalid_tools

let test_find_tool_case_sensitive () =
  (* Tool names are case-sensitive *)
  match find_tool "MASC_INIT" with
  | None -> ()  (* Expected: not found because wrong case *)
  | Some _ -> Alcotest.fail "Tool lookup should be case-sensitive"

(* ============================================================ *)
(* 3. Input Schema Validation Tests                              *)
(* ============================================================ *)

let test_input_schema_type_is_object () =
  List.iter (fun schema ->
    match get_json_string "type" schema.input_schema with
    | Some "object" -> ()
    | Some t -> Alcotest.fail (Printf.sprintf "%s input_schema type is %s, expected object" schema.name t)
    | None -> Alcotest.fail (Printf.sprintf "%s input_schema missing type field" schema.name)
  ) all_schemas

let test_input_schema_has_properties () =
  List.iter (fun schema ->
    match get_json_assoc "properties" schema.input_schema with
    | Some _ -> ()
    | None -> Alcotest.fail (Printf.sprintf "%s input_schema missing properties" schema.name)
  ) all_schemas

let test_required_field_is_list () =
  List.iter (fun schema ->
    match schema.input_schema with
    | `Assoc fields ->
        (match List.assoc_opt "required" fields with
         | None -> ()  (* Optional: some tools have no required fields *)
         | Some (`List _) -> ()
         | Some _ -> Alcotest.fail (Printf.sprintf "%s required field is not a list" schema.name))
    | _ -> Alcotest.fail (Printf.sprintf "%s input_schema is not an object" schema.name)
  ) all_schemas

(* ============================================================ *)
(* 4. Specific Tool Tests                                        *)
(* ============================================================ *)

let test_masc_init_schema () =
  match find_tool "masc_init" with
  | None -> Alcotest.fail "masc_init not found"
  | Some schema ->
      Alcotest.(check bool) "has description" true (String.length schema.description > 10);
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name property" true (List.mem_assoc "agent_name" props)
      | None -> Alcotest.fail "masc_init missing properties"

let test_masc_join_schema () =
  match find_tool "masc_join" with
  | None -> Alcotest.fail "masc_join not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props);
          Alcotest.(check bool) "has capabilities" true (List.mem_assoc "capabilities" props)
      | None -> Alcotest.fail "masc_join missing properties"

let test_masc_leave_schema () =
  match find_tool "masc_leave" with
  | None -> Alcotest.fail "masc_leave not found"
  | Some schema ->
      match get_json_list "required" schema.input_schema with
      | Some reqs ->
          Alcotest.(check bool) "agent_name is required" true
            (List.mem (`String "agent_name") reqs)
      | None -> Alcotest.fail "masc_leave missing required field"

let test_masc_status_schema () =
  match find_tool "masc_status" with
  | None -> Alcotest.fail "masc_status not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          (* masc_status has no required parameters *)
          Alcotest.(check int) "properties can be empty" 0 (List.length props)
      | None -> Alcotest.fail "masc_status missing properties"

let test_masc_broadcast_schema () =
  match find_tool "masc_broadcast" with
  | None -> Alcotest.fail "masc_broadcast not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props);
          Alcotest.(check bool) "has message" true (List.mem_assoc "message" props)
      | None -> Alcotest.fail "masc_broadcast missing properties"

let test_masc_claim_schema () =
  match find_tool "masc_claim" with
  | None -> Alcotest.fail "masc_claim not found"
  | Some schema ->
      match get_json_list "required" schema.input_schema with
      | Some reqs ->
          Alcotest.(check bool) "agent_name required" true (List.mem (`String "agent_name") reqs);
          Alcotest.(check bool) "task_id required" true (List.mem (`String "task_id") reqs)
      | None -> Alcotest.fail "masc_claim missing required field"

let test_masc_add_task_schema () =
  match find_tool "masc_add_task" with
  | None -> Alcotest.fail "masc_add_task not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has title" true (List.mem_assoc "title" props);
          Alcotest.(check bool) "has priority" true (List.mem_assoc "priority" props);
          Alcotest.(check bool) "has description" true (List.mem_assoc "description" props)
      | None -> Alcotest.fail "masc_add_task missing properties"

let test_masc_done_schema () =
  match find_tool "masc_done" with
  | None -> Alcotest.fail "masc_done not found"
  | Some schema ->
      match get_json_list "required" schema.input_schema with
      | Some reqs ->
          Alcotest.(check bool) "agent_name required" true (List.mem (`String "agent_name") reqs);
          Alcotest.(check bool) "task_id required" true (List.mem (`String "task_id") reqs)
      | None -> Alcotest.fail "masc_done missing required field"





(* ============================================================ *)
(* 5. Portal Tool Tests                                          *)
(* ============================================================ *)

let test_masc_portal_open_schema () =
  match find_tool "masc_portal_open" with
  | None -> Alcotest.fail "masc_portal_open not found"
  | Some schema ->
      Alcotest.(check bool) "has portal description" true
        (String.length schema.description > 20)

let test_masc_portal_send_schema () =
  match find_tool "masc_portal_send" with
  | None -> Alcotest.fail "masc_portal_send not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has message" true (List.mem_assoc "message" props)
      | None -> Alcotest.fail "masc_portal_send missing properties"

let test_masc_portal_close_schema () =
  match find_tool "masc_portal_close" with
  | None -> Alcotest.fail "masc_portal_close not found"
  | Some _ -> ()

let test_masc_portal_status_schema () =
  match find_tool "masc_portal_status" with
  | None -> Alcotest.fail "masc_portal_status not found"
  | Some _ -> ()

(* ============================================================ *)
(* 6. Worktree Tool Tests                                        *)
(* ============================================================ *)

let test_masc_worktree_create_schema () =
  match find_tool "masc_worktree_create" with
  | None -> Alcotest.fail "masc_worktree_create not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has task_id" true (List.mem_assoc "task_id" props)
      | None -> Alcotest.fail "masc_worktree_create missing properties"

let test_masc_worktree_remove_schema () =
  match find_tool "masc_worktree_remove" with
  | None -> Alcotest.fail "masc_worktree_remove not found"
  | Some _ -> ()

let test_masc_worktree_list_schema () =
  match find_tool "masc_worktree_list" with
  | None -> Alcotest.fail "masc_worktree_list not found"
  | Some _ -> ()

(* ============================================================ *)
(* 7. Agent Capability Tool Tests                                *)
(* ============================================================ *)

let test_masc_agents_schema () =
  match find_tool "masc_agents" with
  | None -> Alcotest.fail "masc_agents not found"
  | Some _ -> ()

let test_masc_register_capabilities_schema () =
  match find_tool "masc_register_capabilities" with
  | None -> Alcotest.fail "masc_register_capabilities not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props);
          Alcotest.(check bool) "has capabilities" true (List.mem_assoc "capabilities" props)
      | None -> Alcotest.fail "masc_register_capabilities missing properties"

let test_masc_find_by_capability_schema () =
  match find_tool "masc_find_by_capability" with
  | None -> Alcotest.fail "masc_find_by_capability not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has capability" true (List.mem_assoc "capability" props)
      | None -> Alcotest.fail "masc_find_by_capability missing properties"

(* ============================================================ *)
(* 8. Plan Tool Tests                                            *)
(* ============================================================ *)

let test_masc_plan_init_schema () =
  match find_tool "masc_plan_init" with
  | None -> Alcotest.fail "masc_plan_init not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has task_id" true (List.mem_assoc "task_id" props)
      | None -> Alcotest.fail "masc_plan_init missing properties"

let test_masc_plan_update_schema () =
  match find_tool "masc_plan_update" with
  | None -> Alcotest.fail "masc_plan_update not found"
  | Some _ -> ()

let test_masc_plan_get_schema () =
  match find_tool "masc_plan_get" with
  | None -> Alcotest.fail "masc_plan_get not found"
  | Some _ -> ()

let test_masc_deliver_schema () =
  match find_tool "masc_deliver" with
  | None -> Alcotest.fail "masc_deliver not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has content" true (List.mem_assoc "content" props)
      | None -> Alcotest.fail "masc_deliver missing properties"

(* ============================================================ *)
(* 9. Voting Tool Tests                                          *)
(* ============================================================ *)

let test_masc_vote_create_schema () =
  match find_tool "masc_vote_create" with
  | None -> Alcotest.fail "masc_vote_create not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has topic" true (List.mem_assoc "topic" props);
          Alcotest.(check bool) "has options" true (List.mem_assoc "options" props)
      | None -> Alcotest.fail "masc_vote_create missing properties"

let test_masc_vote_cast_schema () =
  match find_tool "masc_vote_cast" with
  | None -> Alcotest.fail "masc_vote_cast not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has vote_id" true (List.mem_assoc "vote_id" props);
          Alcotest.(check bool) "has choice" true (List.mem_assoc "choice" props)
      | None -> Alcotest.fail "masc_vote_cast missing properties"

let test_masc_vote_status_schema () =
  match find_tool "masc_vote_status" with
  | None -> Alcotest.fail "masc_vote_status not found"
  | Some _ -> ()

let test_masc_votes_schema () =
  match find_tool "masc_votes" with
  | None -> Alcotest.fail "masc_votes not found"
  | Some _ -> ()

(* ============================================================ *)
(* 10. Auth Tool Tests                                           *)
(* ============================================================ *)

let test_masc_auth_enable_schema () =
  match find_tool "masc_auth_enable" with
  | None -> Alcotest.fail "masc_auth_enable not found"
  | Some _ -> ()

let test_masc_auth_disable_schema () =
  match find_tool "masc_auth_disable" with
  | None -> Alcotest.fail "masc_auth_disable not found"
  | Some _ -> ()

let test_masc_auth_create_token_schema () =
  match find_tool "masc_auth_create_token" with
  | None -> Alcotest.fail "masc_auth_create_token not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props)
      | None -> Alcotest.fail "masc_auth_create_token missing properties"

(* ============================================================ *)
(* 11. A2A Tool Tests                                            *)
(* ============================================================ *)

let test_masc_a2a_discover_schema () =
  match find_tool "masc_a2a_discover" with
  | None -> Alcotest.fail "masc_a2a_discover not found"
  | Some _ -> ()

let test_masc_a2a_delegate_schema () =
  match find_tool "masc_a2a_delegate" with
  | None -> Alcotest.fail "masc_a2a_delegate not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has target_agent" true (List.mem_assoc "target_agent" props);
          Alcotest.(check bool) "has message" true (List.mem_assoc "message" props)
      | None -> Alcotest.fail "masc_a2a_delegate missing properties"

let test_masc_a2a_subscribe_schema () =
  match find_tool "masc_a2a_subscribe" with
  | None -> Alcotest.fail "masc_a2a_subscribe not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has events" true (List.mem_assoc "events" props)
      | None -> Alcotest.fail "masc_a2a_subscribe missing properties"

let test_masc_poll_events_schema () =
  match find_tool "masc_poll_events" with
  | None -> Alcotest.fail "masc_poll_events not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has subscription_id" true (List.mem_assoc "subscription_id" props)
      | None -> Alcotest.fail "masc_poll_events missing properties"

(* ============================================================ *)
(* 12. Mitosis Tool Tests                                        *)
(* ============================================================ *)

let test_masc_mitosis_status_schema () =
  match find_tool "masc_mitosis_status" with
  | None -> Alcotest.fail "masc_mitosis_status not found"
  | Some _ -> ()

let test_masc_mitosis_divide_schema () =
  match find_tool "masc_mitosis_divide" with
  | None -> Alcotest.fail "masc_mitosis_divide not found"
  | Some _ -> ()

let test_masc_spawn_schema () =
  match find_tool "masc_spawn" with
  | None -> Alcotest.fail "masc_spawn not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props);
          Alcotest.(check bool) "has prompt" true (List.mem_assoc "prompt" props)
      | None -> Alcotest.fail "masc_spawn missing properties"

(* ============================================================ *)
(* 13. Cache Tool Tests                                          *)
(* ============================================================ *)

let test_masc_cache_set_schema () =
  match find_tool "masc_cache_set" with
  | None -> Alcotest.fail "masc_cache_set not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has key" true (List.mem_assoc "key" props);
          Alcotest.(check bool) "has value" true (List.mem_assoc "value" props)
      | None -> Alcotest.fail "masc_cache_set missing properties"

let test_masc_cache_get_schema () =
  match find_tool "masc_cache_get" with
  | None -> Alcotest.fail "masc_cache_get not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has key" true (List.mem_assoc "key" props)
      | None -> Alcotest.fail "masc_cache_get missing properties"

let test_masc_cache_delete_schema () =
  match find_tool "masc_cache_delete" with
  | None -> Alcotest.fail "masc_cache_delete not found"
  | Some _ -> ()

let test_masc_cache_list_schema () =
  match find_tool "masc_cache_list" with
  | None -> Alcotest.fail "masc_cache_list not found"
  | Some _ -> ()

let test_masc_cache_stats_schema () =
  match find_tool "masc_cache_stats" with
  | None -> Alcotest.fail "masc_cache_stats not found"
  | Some _ -> ()

(* ============================================================ *)
(* 14. Handover Tool Tests                                       *)
(* ============================================================ *)

let test_masc_handover_create_schema () =
  match find_tool "masc_handover_create" with
  | None -> Alcotest.fail "masc_handover_create not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has goal" true (List.mem_assoc "goal" props)
      | None -> Alcotest.fail "masc_handover_create missing properties"

let test_masc_handover_list_schema () =
  match find_tool "masc_handover_list" with
  | None -> Alcotest.fail "masc_handover_list not found"
  | Some _ -> ()

let test_masc_handover_claim_schema () =
  match find_tool "masc_handover_claim" with
  | None -> Alcotest.fail "masc_handover_claim not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has handover_id" true (List.mem_assoc "handover_id" props)
      | None -> Alcotest.fail "masc_handover_claim missing properties"

(* ============================================================ *)
(* 15. Swarm Tool Tests                                          *)
(* ============================================================ *)

let test_masc_swarm_init_schema () =
  match find_tool "masc_swarm_init" with
  | None -> Alcotest.fail "masc_swarm_init not found"
  | Some _ -> ()

let test_masc_swarm_join_schema () =
  match find_tool "masc_swarm_join" with
  | None -> Alcotest.fail "masc_swarm_join not found"
  | Some _ -> ()

let test_masc_swarm_leave_schema () =
  match find_tool "masc_swarm_leave" with
  | None -> Alcotest.fail "masc_swarm_leave not found"
  | Some _ -> ()

let test_masc_swarm_status_schema () =
  match find_tool "masc_swarm_status" with
  | None -> Alcotest.fail "masc_swarm_status not found"
  | Some _ -> ()

let test_masc_swarm_propose_schema () =
  match find_tool "masc_swarm_propose" with
  | None -> Alcotest.fail "masc_swarm_propose not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has description" true (List.mem_assoc "description" props)
      | None -> Alcotest.fail "masc_swarm_propose missing properties"

(* ============================================================ *)
(* 16. Walph Tool Tests                                          *)
(* ============================================================ *)

let test_masc_walph_loop_schema () =
  match find_tool "masc_walph_loop" with
  | None -> Alcotest.fail "masc_walph_loop not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props);
          Alcotest.(check bool) "has preset" true (List.mem_assoc "preset" props)
      | None -> Alcotest.fail "masc_walph_loop missing properties"

let test_masc_walph_control_schema () =
  match find_tool "masc_walph_control" with
  | None -> Alcotest.fail "masc_walph_control not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has command" true (List.mem_assoc "command" props)
      | None -> Alcotest.fail "masc_walph_control missing properties"

let test_masc_walph_natural_schema () =
  match find_tool "masc_walph_natural" with
  | None -> Alcotest.fail "masc_walph_natural not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has message" true (List.mem_assoc "message" props)
      | None -> Alcotest.fail "masc_walph_natural missing properties"

(* ============================================================ *)
(* 17. Hat Tool Tests                                            *)
(* ============================================================ *)

let test_masc_hat_wear_schema () =
  match find_tool "masc_hat_wear" with
  | None -> Alcotest.fail "masc_hat_wear not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has hat" true (List.mem_assoc "hat" props);
          Alcotest.(check bool) "has agent_name" true (List.mem_assoc "agent_name" props)
      | None -> Alcotest.fail "masc_hat_wear missing properties"

let test_masc_hat_status_schema () =
  match find_tool "masc_hat_status" with
  | None -> Alcotest.fail "masc_hat_status not found"
  | Some _ -> ()

(* ============================================================ *)
(* 18. Bounded Run Tool Tests                                    *)
(* ============================================================ *)

let test_masc_bounded_run_schema () =
  match find_tool "masc_bounded_run" with
  | None -> Alcotest.fail "masc_bounded_run not found"
  | Some schema ->
      match get_json_assoc "properties" schema.input_schema with
      | Some props ->
          Alcotest.(check bool) "has agents" true (List.mem_assoc "agents" props);
          Alcotest.(check bool) "has prompt" true (List.mem_assoc "prompt" props);
          Alcotest.(check bool) "has constraints" true (List.mem_assoc "constraints" props);
          Alcotest.(check bool) "has goal" true (List.mem_assoc "goal" props)
      | None -> Alcotest.fail "masc_bounded_run missing properties"

(* ============================================================ *)
(* 19. Dashboard Tool Tests                                      *)
(* ============================================================ *)

let test_masc_dashboard_schema () =
  match find_tool "masc_dashboard" with
  | None -> Alcotest.fail "masc_dashboard not found"
  | Some _ -> ()

let test_masc_agent_fitness_schema () =
  match find_tool "masc_agent_fitness" with
  | None -> Alcotest.fail "masc_agent_fitness not found"
  | Some _ -> ()

let test_masc_get_metrics_schema () =
  match find_tool "masc_get_metrics" with
  | None -> Alcotest.fail "masc_get_metrics not found"
  | Some _ -> ()

(* ============================================================ *)
(* 20. Edge Case Tests                                           *)
(* ============================================================ *)

let test_description_not_too_short () =
  List.iter (fun schema ->
    Alcotest.(check bool) (Printf.sprintf "%s description >= 20 chars" schema.name)
      true (String.length schema.description >= 20)
  ) all_schemas

let test_description_not_too_long () =
  List.iter (fun schema ->
    (* Description should be reasonable length for LLM context *)
    Alcotest.(check bool) (Printf.sprintf "%s description <= 1000 chars" schema.name)
      true (String.length schema.description <= 1000)
  ) all_schemas

let test_no_duplicate_properties () =
  List.iter (fun schema ->
    match get_json_assoc "properties" schema.input_schema with
    | Some props ->
        let prop_names = List.map fst props in
        let unique_names = List.sort_uniq String.compare prop_names in
        Alcotest.(check int) (Printf.sprintf "%s no duplicate properties" schema.name)
          (List.length prop_names) (List.length unique_names)
    | None -> ()
  ) all_schemas

let test_property_types_valid () =
  let valid_types = ["string"; "integer"; "number"; "boolean"; "array"; "object"] in
  List.iter (fun schema ->
    match get_json_assoc "properties" schema.input_schema with
    | Some props ->
        List.iter (fun (name, prop_def) ->
          match get_json_string "type" prop_def with
          | Some t ->
              Alcotest.(check bool)
                (Printf.sprintf "%s.%s has valid type %s" schema.name name t)
                true (List.mem t valid_types)
          | None -> ()  (* Type might be inferred or use enum *)
        ) props
    | None -> ()
  ) all_schemas

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run "Tools Coverage" [
    "schema_structure", [
      Alcotest.test_case "not_empty" `Quick test_all_schemas_not_empty;
      Alcotest.test_case "count" `Quick test_all_schemas_count;
      Alcotest.test_case "required_fields" `Quick test_schema_has_required_fields;
      Alcotest.test_case "masc_prefix" `Quick test_all_names_start_with_masc;
    ];
    "find_tool", [
      Alcotest.test_case "existing" `Quick test_find_tool_existing;
      Alcotest.test_case "not_found" `Quick test_find_tool_not_found;
      Alcotest.test_case "case_sensitive" `Quick test_find_tool_case_sensitive;
    ];
    "input_schema", [
      Alcotest.test_case "type_is_object" `Quick test_input_schema_type_is_object;
      Alcotest.test_case "has_properties" `Quick test_input_schema_has_properties;
      Alcotest.test_case "required_is_list" `Quick test_required_field_is_list;
    ];
    "core_tools", [
      Alcotest.test_case "masc_init" `Quick test_masc_init_schema;
      Alcotest.test_case "masc_join" `Quick test_masc_join_schema;
      Alcotest.test_case "masc_leave" `Quick test_masc_leave_schema;
      Alcotest.test_case "masc_status" `Quick test_masc_status_schema;
      Alcotest.test_case "masc_broadcast" `Quick test_masc_broadcast_schema;
      Alcotest.test_case "masc_claim" `Quick test_masc_claim_schema;
      Alcotest.test_case "masc_add_task" `Quick test_masc_add_task_schema;
      Alcotest.test_case "masc_done" `Quick test_masc_done_schema;
    ];
    "portal_tools", [
      Alcotest.test_case "portal_open" `Quick test_masc_portal_open_schema;
      Alcotest.test_case "portal_send" `Quick test_masc_portal_send_schema;
      Alcotest.test_case "portal_close" `Quick test_masc_portal_close_schema;
      Alcotest.test_case "portal_status" `Quick test_masc_portal_status_schema;
    ];
    "worktree_tools", [
      Alcotest.test_case "worktree_create" `Quick test_masc_worktree_create_schema;
      Alcotest.test_case "worktree_remove" `Quick test_masc_worktree_remove_schema;
      Alcotest.test_case "worktree_list" `Quick test_masc_worktree_list_schema;
    ];
    "agent_tools", [
      Alcotest.test_case "agents" `Quick test_masc_agents_schema;
      Alcotest.test_case "register_capabilities" `Quick test_masc_register_capabilities_schema;
      Alcotest.test_case "find_by_capability" `Quick test_masc_find_by_capability_schema;
    ];
    "plan_tools", [
      Alcotest.test_case "plan_init" `Quick test_masc_plan_init_schema;
      Alcotest.test_case "plan_update" `Quick test_masc_plan_update_schema;
      Alcotest.test_case "plan_get" `Quick test_masc_plan_get_schema;
      Alcotest.test_case "deliver" `Quick test_masc_deliver_schema;
    ];
    "vote_tools", [
      Alcotest.test_case "vote_create" `Quick test_masc_vote_create_schema;
      Alcotest.test_case "vote_cast" `Quick test_masc_vote_cast_schema;
      Alcotest.test_case "vote_status" `Quick test_masc_vote_status_schema;
      Alcotest.test_case "votes" `Quick test_masc_votes_schema;
    ];
    "auth_tools", [
      Alcotest.test_case "auth_enable" `Quick test_masc_auth_enable_schema;
      Alcotest.test_case "auth_disable" `Quick test_masc_auth_disable_schema;
      Alcotest.test_case "auth_create_token" `Quick test_masc_auth_create_token_schema;
    ];
    "a2a_tools", [
      Alcotest.test_case "a2a_discover" `Quick test_masc_a2a_discover_schema;
      Alcotest.test_case "a2a_delegate" `Quick test_masc_a2a_delegate_schema;
      Alcotest.test_case "a2a_subscribe" `Quick test_masc_a2a_subscribe_schema;
      Alcotest.test_case "poll_events" `Quick test_masc_poll_events_schema;
    ];
    "mitosis_tools", [
      Alcotest.test_case "mitosis_status" `Quick test_masc_mitosis_status_schema;
      Alcotest.test_case "mitosis_divide" `Quick test_masc_mitosis_divide_schema;
      Alcotest.test_case "spawn" `Quick test_masc_spawn_schema;
    ];
    "cache_tools", [
      Alcotest.test_case "cache_set" `Quick test_masc_cache_set_schema;
      Alcotest.test_case "cache_get" `Quick test_masc_cache_get_schema;
      Alcotest.test_case "cache_delete" `Quick test_masc_cache_delete_schema;
      Alcotest.test_case "cache_list" `Quick test_masc_cache_list_schema;
      Alcotest.test_case "cache_stats" `Quick test_masc_cache_stats_schema;
    ];
    "handover_tools", [
      Alcotest.test_case "handover_create" `Quick test_masc_handover_create_schema;
      Alcotest.test_case "handover_list" `Quick test_masc_handover_list_schema;
      Alcotest.test_case "handover_claim" `Quick test_masc_handover_claim_schema;
    ];
    "swarm_tools", [
      Alcotest.test_case "swarm_init" `Quick test_masc_swarm_init_schema;
      Alcotest.test_case "swarm_join" `Quick test_masc_swarm_join_schema;
      Alcotest.test_case "swarm_leave" `Quick test_masc_swarm_leave_schema;
      Alcotest.test_case "swarm_status" `Quick test_masc_swarm_status_schema;
      Alcotest.test_case "swarm_propose" `Quick test_masc_swarm_propose_schema;
    ];
    "walph_tools", [
      Alcotest.test_case "walph_loop" `Quick test_masc_walph_loop_schema;
      Alcotest.test_case "walph_control" `Quick test_masc_walph_control_schema;
      Alcotest.test_case "walph_natural" `Quick test_masc_walph_natural_schema;
    ];
    "hat_tools", [
      Alcotest.test_case "hat_wear" `Quick test_masc_hat_wear_schema;
      Alcotest.test_case "hat_status" `Quick test_masc_hat_status_schema;
    ];
    "bounded_run", [
      Alcotest.test_case "bounded_run" `Quick test_masc_bounded_run_schema;
    ];
    "dashboard_tools", [
      Alcotest.test_case "dashboard" `Quick test_masc_dashboard_schema;
      Alcotest.test_case "agent_fitness" `Quick test_masc_agent_fitness_schema;
      Alcotest.test_case "get_metrics" `Quick test_masc_get_metrics_schema;
    ];
    "edge_cases", [
      Alcotest.test_case "description_not_short" `Quick test_description_not_too_short;
      Alcotest.test_case "description_not_long" `Quick test_description_not_too_long;
      Alcotest.test_case "no_duplicate_props" `Quick test_no_duplicate_properties;
      Alcotest.test_case "valid_prop_types" `Quick test_property_types_valid;
    ];
  ]
