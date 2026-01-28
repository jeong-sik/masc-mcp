(** Datachannel Eio Module Coverage Tests

    Tests for datachannel configuration:
    - config type
    - default_config function
*)

open Alcotest

module Datachannel_eio = Masc_mcp.Datachannel_eio

(* ============================================================
   config Type Tests
   ============================================================ *)

let test_config_type () =
  let dc_cfg = Masc_mcp.Datachannel.default_config in
  let sctp_cfg = Masc_mcp.Sctp.default_config in
  let c : Datachannel_eio.config = {
    dc_config = dc_cfg;
    sctp_config = sctp_cfg;
    is_offerer = true;
  } in
  check bool "is_offerer" true c.is_offerer

let test_config_answerer () =
  let c : Datachannel_eio.config = {
    dc_config = Masc_mcp.Datachannel.default_config;
    sctp_config = Masc_mcp.Sctp.default_config;
    is_offerer = false;
  } in
  check bool "is_offerer" false c.is_offerer

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_offerer () =
  let cfg = Datachannel_eio.default_config ~is_offerer:true in
  check bool "is_offerer true" true cfg.is_offerer

let test_default_config_answerer () =
  let cfg = Datachannel_eio.default_config ~is_offerer:false in
  check bool "is_offerer false" false cfg.is_offerer

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Datachannel Eio Coverage" [
    "config", [
      test_case "type" `Quick test_config_type;
      test_case "answerer" `Quick test_config_answerer;
    ];
    "default_config", [
      test_case "offerer" `Quick test_default_config_offerer;
      test_case "answerer" `Quick test_default_config_answerer;
    ];
  ]
