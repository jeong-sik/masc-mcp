(** MASC Logging System - Structured logging with levels *)

(** Log levels *)
type level =
  | Debug
  | Info
  | Warn
  | Error

(** Current log level (mutable, can be set at runtime) *)
let current_level = ref Info

(** Level to string *)
let level_to_string = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

(** Level to int for comparison *)
let level_to_int = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3

(** Parse level from string *)
let level_of_string s =
  match String.lowercase_ascii s with
  | "debug" -> Debug
  | "info" -> Info
  | "warn" | "warning" -> Warn
  | "error" -> Error
  | _ -> Info  (* Default to Info *)

(** Check if level should be logged *)
let should_log level =
  level_to_int level >= level_to_int !current_level

(** Set log level *)
let set_level level =
  current_level := level

(** Set log level from string (e.g., from env var) *)
let set_level_from_string s =
  current_level := level_of_string s

(** Initialize from MASC_LOG_LEVEL env var *)
let init_from_env () =
  match Sys.getenv_opt "MASC_LOG_LEVEL" with
  | Some s -> set_level_from_string s
  | None -> ()

(** Get current timestamp *)
let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Log a message at given level with optional context *)
let log level ?(ctx : string option) fmt =
  Printf.ksprintf (fun msg ->
    if should_log level then begin
      let prefix = match ctx with
        | Some c -> Printf.sprintf "[%s] [%s] [%s]" (timestamp ()) (level_to_string level) c
        | None -> Printf.sprintf "[%s] [%s]" (timestamp ()) (level_to_string level)
      in
      Printf.eprintf "%s %s\n%!" prefix msg
    end
  ) fmt

(** Convenience functions *)
let debug ?ctx fmt = log Debug ?ctx fmt
let info ?ctx fmt = log Info ?ctx fmt
let warn ?ctx fmt = log Warn ?ctx fmt
let error ?ctx fmt = log Error ?ctx fmt

(** Module-specific loggers *)
module Make (M : sig val name : string end) = struct
  let debug fmt = log Debug ~ctx:M.name fmt
  let info fmt = log Info ~ctx:M.name fmt
  let warn fmt = log Warn ~ctx:M.name fmt
  let error fmt = log Error ~ctx:M.name fmt
end

(** Pre-defined module loggers *)
module Room = Make(struct let name = "Room" end)
module Mcp = Make(struct let name = "MCP" end)
module Auth = Make(struct let name = "Auth" end)
module Retry = Make(struct let name = "Retry" end)
module Backend = Make(struct let name = "Backend" end)
module Session = Make(struct let name = "Session" end)
module Cancel = Make(struct let name = "Cancellation" end)
module Sub = Make(struct let name = "Subscriptions" end)
