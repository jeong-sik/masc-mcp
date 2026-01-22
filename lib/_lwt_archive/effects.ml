(** P1 Effect System for MASC MCP

    Based on:
    - R7: Jane Street "Effective Programming: OCaml Effects" (2024)
    - R8: "Retrofitting Effect Handlers onto OCaml" ACM PLDI (2021)
    - R11: "Active Objects with Algebraic Effects" Springer (2024)

    OCaml 5.3+ Effect System implementation for type-safe side effect tracking.
    All side effects are explicitly tracked in types, enabling:
    - Deterministic testing with mock handlers
    - Clear separation of pure business logic and I/O
    - Composable effect handlers for production vs test environments
*)

(** {1 Effect Declarations} *)

(** File system effects *)
type _ Effect.t +=
  | Read_file : string -> string Effect.t
  | Write_file : (string * string) -> unit Effect.t
  | File_exists : string -> bool Effect.t
  | Remove_file : string -> unit Effect.t
  | List_dir : string -> string list Effect.t

(** Time effects *)
type _ Effect.t +=
  | Get_time : unit -> float Effect.t
  | Sleep : float -> unit Effect.t

(** Logging effects *)
type _ Effect.t +=
  | Log_debug : string -> unit Effect.t
  | Log_info : string -> unit Effect.t
  | Log_warn : string -> unit Effect.t
  | Log_error : string -> unit Effect.t

(** HTTP effects *)
type _ Effect.t +=
  | Http_get : string -> string Effect.t
  | Http_post : (string * string) -> string Effect.t

(** Random effects (for deterministic testing) *)
type _ Effect.t +=
  | Random_float : float -> float Effect.t
  | Random_int : int -> int Effect.t

(** Environment effects *)
type _ Effect.t +=
  | Get_env : string -> string option Effect.t


(** {1 Pure Module - No Effects} *)

(** Pure business logic without side effects.
    All functions here are referentially transparent and deterministic. *)
module Pure = struct

  (** Calculate context usage threshold for relay decision.
      @param messages Number of messages in conversation
      @param tool_calls Number of tool calls made
      @return Threshold value between 0.0 and 1.0 *)
  let calculate_threshold ~messages ~tool_calls =
    let base = 0.7 in
    let msg_factor = float_of_int messages *. 0.001 in
    let tool_factor = float_of_int tool_calls *. 0.005 in
    min 1.0 (base +. msg_factor +. tool_factor)

  (** Determine if relay should be triggered.
      @param usage Current context usage ratio (0.0-1.0)
      @param threshold Trigger threshold (0.0-1.0)
      @return true if relay should be triggered *)
  let should_relay ~usage ~threshold =
    usage > threshold

  (** Compress context to fit within token limit.
      @param context Full context string
      @param max_tokens Maximum allowed tokens/characters
      @return Compressed context *)
  let compress_context ~context ~max_tokens =
    if String.length context <= max_tokens then context
    else String.sub context 0 max_tokens

  (** Calculate fitness score from agent metrics.
      Weights based on R3/R4 evolutionary selection principles. *)
  let calculate_fitness_score
      ~task_completion_rate
      ~avg_response_time
      ~error_rate
      ~handoff_success_rate
      ~collaboration_score =
    let weights = (0.35, 0.15, 0.25, 0.15, 0.10) in
    let (w_complete, w_speed, w_reliable, w_handoff, w_collab) = weights in
    (* Normalize response time: faster is better, cap at 60 seconds *)
    let speed_score = 1.0 -. (min 1.0 (avg_response_time /. 60.0)) in
    let reliability_score = 1.0 -. error_rate in
    (task_completion_rate *. w_complete) +.
    (speed_score *. w_speed) +.
    (reliability_score *. w_reliable) +.
    (handoff_success_rate *. w_handoff) +.
    (collaboration_score *. w_collab)

  (** Hebbian weight update: strengthen on success.
      @param current_weight Current synapse weight (0.0-1.0)
      @param learning_rate Learning rate (typically 0.1 for fast, 0.01 for slow)
      @return New weight capped at 1.0 *)
  let hebbian_strengthen ~current_weight ~learning_rate =
    let delta = learning_rate *. (1.0 -. current_weight) in
    min 1.0 (current_weight +. delta)

  (** Hebbian weight update: weaken on failure.
      @param current_weight Current synapse weight (0.0-1.0)
      @param learning_rate Learning rate
      @return New weight floored at 0.0 *)
  let hebbian_weaken ~current_weight ~learning_rate =
    let delta = learning_rate *. current_weight in
    max 0.0 (current_weight -. delta)

  (** Calculate cosine similarity between two vectors.
      Used for drift detection. *)
  let cosine_similarity v1 v2 =
    let n1 = Array.length v1 in
    let n2 = Array.length v2 in
    if n1 <> n2 then 0.0
    else if n1 = 0 then 0.0
    else begin
      let dot = ref 0.0 in
      let norm1 = ref 0.0 in
      let norm2 = ref 0.0 in
      for i = 0 to n1 - 1 do
        let a = v1.(i) in
        let b = v2.(i) in
        dot := !dot +. (a *. b);
        norm1 := !norm1 +. (a *. a);
        norm2 := !norm2 +. (b *. b)
      done;
      let n1_sqrt = sqrt !norm1 in
      let n2_sqrt = sqrt !norm2 in
      if n1_sqrt = 0.0 || n2_sqrt = 0.0 then 0.0
      else !dot /. (n1_sqrt *. n2_sqrt)
    end

  (** Check if drift is detected based on similarity threshold. *)
  let is_drift_detected ~similarity ~threshold =
    similarity < threshold

  (** Parse JSON safely - returns None on failure *)
  let parse_json_opt json_str =
    try Some (Yojson.Safe.from_string json_str)
    with _ -> None

  (** Serialize to JSON *)
  let to_json_string value =
    Yojson.Safe.to_string value
end


(** {1 Effectful Module - Tracked Side Effects} *)

(** Effectful operations that perform I/O.
    All effects are explicitly tracked via the effect system. *)
module Effectful = struct

  (** Read agent state from filesystem.
      Effect: Read_file *)
  let read_agent_state agent_id =
    let path = Printf.sprintf ".masc/agents/%s.json" agent_id in
    Effect.perform (Read_file path)

  (** Save task to filesystem.
      Effect: Write_file *)
  let save_task ~task_id ~content =
    let path = Printf.sprintf ".masc/tasks/%s.json" task_id in
    Effect.perform (Write_file (path, content))

  (** Check if agent exists.
      Effect: File_exists *)
  let agent_exists agent_id =
    let path = Printf.sprintf ".masc/agents/%s.json" agent_id in
    Effect.perform (File_exists path)

  (** Log a message with timestamp.
      Effects: Get_time, Log_info *)
  let log_with_timestamp msg =
    let now = Effect.perform (Get_time ()) in
    let formatted = Printf.sprintf "[%.3f] %s" now msg in
    Effect.perform (Log_info formatted)

  (** Broadcast message - log to file.
      Effects: Get_time, Write_file *)
  let broadcast ~room_path message =
    let now = Effect.perform (Get_time ()) in
    let log_line = Printf.sprintf "%.3f: %s\n" now message in
    let log_path = Printf.sprintf "%s/messages.log" room_path in
    Effect.perform (Write_file (log_path, log_line))

  (** Get current timestamp.
      Effect: Get_time *)
  let get_timestamp () =
    Effect.perform (Get_time ())

  (** Random agent selection for task distribution.
      Effect: Random_int *)
  let select_random_agent agents =
    let n = List.length agents in
    if n = 0 then None
    else
      let idx = Effect.perform (Random_int n) in
      Some (List.nth agents idx)

  (** Read environment variable.
      Effect: Get_env *)
  let get_env_var name =
    Effect.perform (Get_env name)
end


(** {1 Effect Handlers} *)

(** Production handler: performs real I/O operations. *)
let run_with_filesystem computation =
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = raise;
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Read_file path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            let content = In_channel.with_open_text path In_channel.input_all in
            Effect.Deep.continue k content)

        | Write_file (path, content) ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* Ensure parent directory exists *)
            let dir = Filename.dirname path in
            if not (Sys.file_exists dir) then
              Sys.mkdir dir 0o755;
            Out_channel.with_open_text path (fun oc ->
              Out_channel.output_string oc content);
            Effect.Deep.continue k ())

        | File_exists path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Sys.file_exists path))

        | Remove_file path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Sys.remove path;
            Effect.Deep.continue k ())

        | List_dir path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            let entries = Sys.readdir path |> Array.to_list in
            Effect.Deep.continue k entries)

        | Get_time () ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Unix.gettimeofday ()))

        | Sleep duration ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Unix.sleepf duration;
            Effect.Deep.continue k ())

        | Log_debug msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Printf.eprintf "[DEBUG] %s\n%!" msg;
            Effect.Deep.continue k ())

        | Log_info msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Printf.eprintf "[INFO] %s\n%!" msg;
            Effect.Deep.continue k ())

        | Log_warn msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Printf.eprintf "[WARN] %s\n%!" msg;
            Effect.Deep.continue k ())

        | Log_error msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Printf.eprintf "[ERROR] %s\n%!" msg;
            Effect.Deep.continue k ())

        | Random_float bound ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Random.float bound))

        | Random_int bound ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Random.int bound))

        | Get_env name ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Sys.getenv_opt name))

        (* HTTP effects require external dependencies, return empty for now *)
        | Http_get _ ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k "")

        | Http_post _ ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k "")

        | _ -> None
    }


(** Mock filesystem for testing: in-memory key-value store *)
type mock_fs = (string, string) Hashtbl.t

(** Create a new mock filesystem *)
let create_mock_fs () : mock_fs = Hashtbl.create 16

(** Test handler: uses mock filesystem, deterministic time/random.
    Enables fully deterministic testing without real I/O. *)
let run_with_mock ~(mock_fs : mock_fs) ~(fixed_time : float) computation =
  let log_buffer = Buffer.create 256 in
  Effect.Deep.match_with computation ()
    { retc = (fun x -> x);
      exnc = raise;
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Read_file path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            let content =
              Hashtbl.find_opt mock_fs path
              |> Option.value ~default:""
            in
            Effect.Deep.continue k content)

        | Write_file (path, content) ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Hashtbl.replace mock_fs path content;
            Effect.Deep.continue k ())

        | File_exists path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k (Hashtbl.mem mock_fs path))

        | Remove_file path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Hashtbl.remove mock_fs path;
            Effect.Deep.continue k ())

        | List_dir _path ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* Return all keys as mock directory listing *)
            let entries = Hashtbl.fold (fun k _ acc -> k :: acc) mock_fs [] in
            Effect.Deep.continue k entries)

        | Get_time () ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k fixed_time)

        | Sleep _duration ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* No-op in tests *)
            Effect.Deep.continue k ())

        | Log_debug msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Buffer.add_string log_buffer (Printf.sprintf "[DEBUG] %s\n" msg);
            Effect.Deep.continue k ())

        | Log_info msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Buffer.add_string log_buffer (Printf.sprintf "[INFO] %s\n" msg);
            Effect.Deep.continue k ())

        | Log_warn msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Buffer.add_string log_buffer (Printf.sprintf "[WARN] %s\n" msg);
            Effect.Deep.continue k ())

        | Log_error msg ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Buffer.add_string log_buffer (Printf.sprintf "[ERROR] %s\n" msg);
            Effect.Deep.continue k ())

        | Random_float _bound ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* Deterministic: always return 0.5 scaled *)
            Effect.Deep.continue k 0.5)

        | Random_int bound ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* Deterministic: always return middle value *)
            Effect.Deep.continue k (bound / 2))

        | Get_env name ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            (* Mock environment - check mock_fs for env:NAME keys *)
            let env_key = "env:" ^ name in
            Effect.Deep.continue k (Hashtbl.find_opt mock_fs env_key))

        | Http_get _ ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k "{\"mock\": true}")

        | Http_post _ ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k "{\"mock\": true}")

        | _ -> None
    }


(** {1 Convenience Functions} *)

(** Run pure computation (no effects needed) *)
let run_pure f = f ()

(** Run effectful computation with production handlers *)
let run f = run_with_filesystem f

(** Run effectful computation with test handlers *)
let run_test ?(mock_fs = create_mock_fs ()) ?(fixed_time = 1704067200.0) f =
  run_with_mock ~mock_fs ~fixed_time f


(** {1 Integration with Lwt} *)

(** Type alias for Lwt-based effect results.
    For gradual migration from Lwt to effects. *)
type 'a effect_result = 'a

(** Wrap effectful computation for Lwt compatibility.
    This allows mixing effect-based and Lwt-based code during migration. *)
let to_lwt computation =
  Lwt.return (run computation)

(** Wrap effectful computation with mock for Lwt-based tests. *)
let to_lwt_test ?mock_fs ?fixed_time computation =
  Lwt.return (run_test ?mock_fs ?fixed_time computation)
