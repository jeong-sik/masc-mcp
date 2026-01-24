(** MASC Configuration Management

    Persists mode settings to .masc/config.json
*)

open Mode

(** Configuration record *)
type t = {
  mode : mode;
  enabled_categories : category list;
}

(** Default configuration *)
let default = {
  mode = Standard;
  enabled_categories = categories_for_mode Standard;
}

(** Config file name *)
let config_filename = "config.json"

(** Get config file path *)
let config_path room_path =
  Filename.concat room_path config_filename

(** Convert config to JSON *)
let to_json config =
  `Assoc [
    ("mode", `String (mode_to_string config.mode));
    ("enabled_categories", categories_to_json config.enabled_categories);
  ]

(** Parse config from JSON *)
let of_json json =
  try
    let mode_str =
      match Yojson.Safe.Util.member "mode" json with
      | `String s -> s
      | _ -> "standard"
    in
    let mode =
      match mode_of_string mode_str with
      | Some m -> m
      | None -> Standard
    in
    let enabled_categories =
      match mode with
      | Custom ->
        let cats_json = Yojson.Safe.Util.member "enabled_categories" json in
        categories_of_json cats_json
      | _ -> categories_for_mode mode
    in
    { mode; enabled_categories }
  with _ -> default

(** Load config from file *)
let load room_path =
  let path = config_path room_path in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      of_json json
    with _ -> default
  else
    default

(** Save config to file *)
let save room_path config =
  let path = config_path room_path in
  let json = to_json config in
  let content = Yojson.Safe.pretty_to_string json in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Switch to a preset mode *)
let switch_mode room_path mode =
  let enabled_categories = categories_for_mode mode in
  let config = { mode; enabled_categories } in
  save room_path config;
  config

(** Enable specific categories (switches to Custom mode) *)
let set_categories room_path categories =
  let config = { mode = Custom; enabled_categories = categories } in
  save room_path config;
  config

(** Enable a category *)
let enable_category room_path category =
  let current = load room_path in
  let new_cats =
    if List.mem category current.enabled_categories then
      current.enabled_categories
    else
      category :: current.enabled_categories
  in
  set_categories room_path new_cats

(** Disable a category *)
let disable_category room_path category =
  let current = load room_path in
  let new_cats = List.filter (fun c -> c <> category) current.enabled_categories in
  set_categories room_path new_cats

(** Get current config summary as JSON for tool response *)
let get_config_summary room_path =
  let config = load room_path in
  let enabled_names = List.map category_to_string config.enabled_categories in
  let disabled = List.filter (fun c -> not (List.mem c config.enabled_categories)) all_categories in
  let disabled_names = List.map category_to_string disabled in
  let tool_count =
    let enabled =
      List.filter
        (fun (schema : Types.tool_schema) ->
          Mode.is_tool_enabled config.enabled_categories schema.name)
        Tools.all_schemas
    in
    List.length enabled
  in
  `Assoc [
    ("mode", `String (mode_to_string config.mode));
    ("mode_description", `String (mode_description config.mode));
    ("enabled_categories", `List (List.map (fun s -> `String s) enabled_names));
    ("disabled_categories", `List (List.map (fun s -> `String s) disabled_names));
    ("enabled_tool_count", `Int tool_count);
    ("available_modes", `List [
      `Assoc [("name", `String "minimal"); ("description", `String (mode_description Minimal))];
      `Assoc [("name", `String "standard"); ("description", `String (mode_description Standard))];
      `Assoc [("name", `String "parallel"); ("description", `String (mode_description Parallel))];
      `Assoc [("name", `String "full"); ("description", `String (mode_description Full))];
      `Assoc [("name", `String "solo"); ("description", `String (mode_description Solo))];
    ]);
  ]
