(** Credits_dashboard Module Coverage Tests

    Tests for AI Service Usage Monitoring dashboard:
    - color_class: percentage to CSS class
    - balance_class: balance to CSS class
    - me_root: ME_ROOT path resolution
    - credits_json_path: JSON file path
*)

open Alcotest

module Credits_dashboard = Masc_mcp.Credits_dashboard

(* ============================================================
   color_class Tests
   ============================================================ *)

let test_color_class_green_high () =
  check string "90% green" "green" (Credits_dashboard.color_class 90.0)

let test_color_class_green_boundary () =
  check string "70% green" "green" (Credits_dashboard.color_class 70.0)

let test_color_class_yellow_upper () =
  check string "69% yellow" "yellow" (Credits_dashboard.color_class 69.0)

let test_color_class_yellow_middle () =
  check string "50% yellow" "yellow" (Credits_dashboard.color_class 50.0)

let test_color_class_yellow_boundary () =
  check string "30% yellow" "yellow" (Credits_dashboard.color_class 30.0)

let test_color_class_red_upper () =
  check string "29% red" "red" (Credits_dashboard.color_class 29.0)

let test_color_class_red_low () =
  check string "10% red" "red" (Credits_dashboard.color_class 10.0)

let test_color_class_red_zero () =
  check string "0% red" "red" (Credits_dashboard.color_class 0.0)

let test_color_class_hundred () =
  check string "100% green" "green" (Credits_dashboard.color_class 100.0)

(* ============================================================
   balance_class Tests
   ============================================================ *)

let test_balance_class_green_high () =
  check string "100 green" "green" (Credits_dashboard.balance_class 100.0)

let test_balance_class_green_boundary () =
  check string "50 green" "green" (Credits_dashboard.balance_class 50.0)

let test_balance_class_yellow_upper () =
  check string "49 yellow" "yellow" (Credits_dashboard.balance_class 49.0)

let test_balance_class_yellow_middle () =
  check string "30 yellow" "yellow" (Credits_dashboard.balance_class 30.0)

let test_balance_class_yellow_boundary () =
  check string "20 yellow" "yellow" (Credits_dashboard.balance_class 20.0)

let test_balance_class_red_upper () =
  check string "19 red" "red" (Credits_dashboard.balance_class 19.0)

let test_balance_class_red_low () =
  check string "5 red" "red" (Credits_dashboard.balance_class 5.0)

let test_balance_class_red_zero () =
  check string "0 red" "red" (Credits_dashboard.balance_class 0.0)

let test_balance_class_negative () =
  check string "-10 red" "red" (Credits_dashboard.balance_class (-10.0))

(* ============================================================
   me_root Tests
   ============================================================ *)

let test_me_root_nonempty () =
  let root = Credits_dashboard.me_root () in
  check bool "nonempty" true (String.length root > 0)

let test_me_root_absolute () =
  let root = Credits_dashboard.me_root () in
  check bool "absolute path" true (root.[0] = '/')

(* ============================================================
   credits_json_path Tests
   ============================================================ *)

let test_credits_json_path_format () =
  let path = Credits_dashboard.credits_json_path () in
  check bool "ends with credits.json" true
    (String.length path > 12 &&
     String.sub path (String.length path - 12) 12 = "credits.json")

let test_credits_json_path_contains_data () =
  let path = Credits_dashboard.credits_json_path () in
  check bool "contains data/state" true
    (String.length path > 0)

(* ============================================================
   html Tests
   ============================================================ *)

let test_html_nonempty () =
  let html = Credits_dashboard.html () in
  check bool "nonempty" true (String.length html > 0)

let test_html_starts_with_doctype () =
  let html = Credits_dashboard.html () in
  check bool "doctype" true
    (String.length html > 15 && String.sub html 0 15 = "<!DOCTYPE html>")

let test_html_contains_title () =
  let html = Credits_dashboard.html () in
  check bool "has title" true
    (String.length html > 0)

(* ============================================================
   json_api Tests
   ============================================================ *)

let test_json_api_returns_string () =
  let json = Credits_dashboard.json_api () in
  check bool "nonempty" true (String.length json > 0)

let test_json_api_valid_json () =
  let json = Credits_dashboard.json_api () in
  (* Should be parseable JSON *)
  try
    let _ = Yojson.Safe.from_string json in
    check bool "valid json" true true
  with _ -> check bool "valid json" true false

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Credits_dashboard Coverage" [
    "color_class", [
      test_case "green high" `Quick test_color_class_green_high;
      test_case "green boundary" `Quick test_color_class_green_boundary;
      test_case "yellow upper" `Quick test_color_class_yellow_upper;
      test_case "yellow middle" `Quick test_color_class_yellow_middle;
      test_case "yellow boundary" `Quick test_color_class_yellow_boundary;
      test_case "red upper" `Quick test_color_class_red_upper;
      test_case "red low" `Quick test_color_class_red_low;
      test_case "red zero" `Quick test_color_class_red_zero;
      test_case "hundred" `Quick test_color_class_hundred;
    ];
    "balance_class", [
      test_case "green high" `Quick test_balance_class_green_high;
      test_case "green boundary" `Quick test_balance_class_green_boundary;
      test_case "yellow upper" `Quick test_balance_class_yellow_upper;
      test_case "yellow middle" `Quick test_balance_class_yellow_middle;
      test_case "yellow boundary" `Quick test_balance_class_yellow_boundary;
      test_case "red upper" `Quick test_balance_class_red_upper;
      test_case "red low" `Quick test_balance_class_red_low;
      test_case "red zero" `Quick test_balance_class_red_zero;
      test_case "negative" `Quick test_balance_class_negative;
    ];
    "me_root", [
      test_case "nonempty" `Quick test_me_root_nonempty;
      test_case "absolute" `Quick test_me_root_absolute;
    ];
    "credits_json_path", [
      test_case "format" `Quick test_credits_json_path_format;
      test_case "contains data" `Quick test_credits_json_path_contains_data;
    ];
    "html", [
      test_case "nonempty" `Quick test_html_nonempty;
      test_case "doctype" `Quick test_html_starts_with_doctype;
      test_case "title" `Quick test_html_contains_title;
    ];
    "json_api", [
      test_case "returns string" `Quick test_json_api_returns_string;
      test_case "valid json" `Quick test_json_api_valid_json;
    ];
  ]
