(** Safe Operations Module

    Provides safe wrappers for common operations that may fail,
    with proper error handling and logging instead of silent suppression.

    Design principles:
    - Never silently swallow exceptions
    - Always provide context for errors
    - Use Result types for recoverable errors
    - Log unexpected failures for debugging
*)

(** Execute a function, logging exceptions and returning None on failure *)
let try_with_log context f =
  try Some (f ())
  with e ->
    Eio.traceln "[WARN] %s failed: %s" context (Printexc.to_string e);
    None

(** Execute with default value on failure *)
let try_with_default ~default context f =
  match try_with_log context f with
  | Some v -> v
  | None -> default

(** Parse JSON with detailed error reporting *)
let parse_json_safe ~context str : (Yojson.Safe.t, string) result =
  try Ok (Yojson.Safe.from_string str)
  with Yojson.Json_error msg ->
    let preview = if String.length str > 50 then String.sub str 0 50 ^ "..." else str in
    Error (Printf.sprintf "[%s] JSON parse error: %s (input: %s)" context msg preview)

(** Read file contents with error handling *)
let read_file_safe path : (string, string) result =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "File not found: %s" path)
  else
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Ok content
    with e ->
      Error (Printf.sprintf "Failed to read %s: %s" path (Printexc.to_string e))

(** Safe integer parsing *)
let int_of_string_safe str =
  try Some (int_of_string str)
  with Failure _ -> None

(** Integer parsing with default *)
let int_of_string_with_default ~default str =
  match int_of_string_safe str with
  | Some v -> v
  | None -> default

(** Safe float parsing *)
let float_of_string_safe str =
  try Some (float_of_string str)
  with Failure _ -> None

(** Float parsing with default *)
let float_of_string_with_default ~default str =
  match float_of_string_safe str with
  | Some v -> v
  | None -> default

(** Read JSON file safely *)
let read_json_file_safe path : (Yojson.Safe.t, string) result =
  match read_file_safe path with
  | Error e -> Error e
  | Ok content -> parse_json_safe ~context:path content

(** List files in directory safely *)
let list_dir_safe path : (string list, string) result =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "Directory not found: %s" path)
  else if not (Sys.is_directory path) then
    Error (Printf.sprintf "Not a directory: %s" path)
  else
    try Ok (Sys.readdir path |> Array.to_list)
    with e ->
      Error (Printf.sprintf "Failed to list %s: %s" path (Printexc.to_string e))

(** Remove file with logging on failure (for cleanup operations) *)
let remove_file_logged ?(context = "cleanup") path =
  try Sys.remove path
  with e ->
    Eio.traceln "[WARN] [%s] Failed to remove %s: %s" context path (Printexc.to_string e)

(** Close channel with logging on failure *)
let close_in_logged ic =
  try close_in ic
  with e ->
    Eio.traceln "[WARN] Failed to close input channel: %s" (Printexc.to_string e)

(** Get environment variable with logging when invalid *)
let get_env_int_logged name ~default =
  match Sys.getenv_opt name with
  | None -> default
  | Some v ->
    match int_of_string_safe v with
    | Some n -> n
    | None ->
      Eio.traceln "[WARN] Invalid int for %s=%s, using default %d" name v default;
      default

let get_env_float_logged name ~default =
  match Sys.getenv_opt name with
  | None -> default
  | Some v ->
    match float_of_string_safe v with
    | Some n -> n
    | None ->
      Eio.traceln "[WARN] Invalid float for %s=%s, using default %f" name v default;
      default

(** {2 JSON Value Extraction Helpers}

    Safe extraction from Yojson.Safe.t values with proper error handling.
    These replace `with _ -> default` patterns in JSON parsing code.
*)

let json_string ?(default = "") key json =
  let open Yojson.Safe.Util in
  try json |> member key |> to_string
  with Type_error _ -> default

let json_int ?(default = 0) key json =
  let open Yojson.Safe.Util in
  try json |> member key |> to_int
  with Type_error _ -> default

let json_float ?(default = 0.0) key json =
  let open Yojson.Safe.Util in
  try json |> member key |> to_float
  with Type_error _ -> default

let json_bool ?(default = false) key json =
  let open Yojson.Safe.Util in
  try json |> member key |> to_bool
  with Type_error _ -> default

let json_string_list key json =
  let open Yojson.Safe.Util in
  try json |> member key |> to_list |> List.map to_string
  with Type_error _ -> []

let json_string_opt key json =
  let open Yojson.Safe.Util in
  try
    match json |> member key with
    | `Null -> None
    | j -> Some (to_string j)
  with Type_error _ -> None

let json_int_opt key json =
  let open Yojson.Safe.Util in
  try
    match json |> member key with
    | `Null -> None
    | j -> Some (to_int j)
  with Type_error _ -> None
