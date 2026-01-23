(** Session lock concurrency tests (Eio fibers) *)

open Alcotest

module Session = Masc_mcp.Session
module Types = Masc_mcp.Types

let test_concurrent_activity_and_rate_limit () =
  Eio_main.run @@ fun _ ->
  let registry = Session.create () in
  ignore (Session.register registry ~agent_name:"claude");

  let iterations = 200 in
  let worker () =
    for _ = 1 to iterations do
      Session.update_activity registry ~agent_name:"claude" ();
      ignore (Session.check_rate_limit_ex registry ~agent_name:"claude"
                ~category:Types.GeneralLimit ~role:Types.Worker);
    done
  in

  Eio.Fiber.all [worker; worker; worker; worker];

  let status =
    Session.get_rate_limit_status registry ~agent_name:"claude" ~role:Types.Worker
  in
  let open Yojson.Safe.Util in
  let burst_remaining = status |> member "burst_remaining" |> to_int in
  check bool "burst remaining non-negative" true (burst_remaining >= 0);
  check bool "agent still connected" true (List.mem "claude" (Session.connected_agents registry))

let () =
  run "Session_locking" [
    "concurrency", [
      test_case "activity + rate limit" `Quick test_concurrent_activity_and_rate_limit;
    ];
  ]
