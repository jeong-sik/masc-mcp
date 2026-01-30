module Schema = Graphql.Schema
module Arg = Schema.Arg
module Room = Room
module Room_utils = Room_utils
module Types = Types
open Types

type ctx = {
  room_config: Room_utils.config;
}

type response_status = [ `OK | `Bad_request ]

type response = {
  status: response_status;
  body: string;
}

type page_info = {
  has_next_page: bool;
  end_cursor: string option;
}

type 'a edge = {
  node: 'a;
  cursor: string;
}

type 'a connection = {
  edges: 'a edge list;
  page_info: page_info;
  total_count: int;
}

type task_status_info = {
  status: string;
  assignee: string option;
  claimed_at: string option;
  started_at: string option;
  completed_at: string option;
  notes: string option;
  cancelled_by: string option;
  cancelled_at: string option;
  reason: string option;
}

let max_first = 200
let default_first = 50

let encode_cursor ~kind value =
  Base64.encode_string (kind ^ ":" ^ value)

let decode_cursor ~kind cursor =
  match Base64.decode cursor with
  | Ok decoded ->
      let prefix = kind ^ ":" in
      let prefix_len = String.length prefix in
      if String.length decoded >= prefix_len &&
         String.sub decoded 0 prefix_len = prefix then
        Some (String.sub decoded prefix_len (String.length decoded - prefix_len))
      else
        None
  | Error _ -> None

let clamp_first = function
  | None -> default_first
  | Some n -> max 0 (min n max_first)

let rec take n items =
  match items with
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let drop_after_id id_of items after_id =
  match after_id with
  | None -> items
  | Some target ->
      let rec loop = function
        | [] -> []
        | x :: xs when String.equal (id_of x) target -> xs
        | _ :: xs -> loop xs
      in
      loop items

let task_status_info_of_task (task : Types.task) =
  match task.task_status with
  | Types.Todo ->
      {
        status = "todo";
        assignee = None;
        claimed_at = None;
        started_at = None;
        completed_at = None;
        notes = None;
        cancelled_by = None;
        cancelled_at = None;
        reason = None;
      }
  | Types.Claimed { assignee; claimed_at } ->
      {
        status = "claimed";
        assignee = Some assignee;
        claimed_at = Some claimed_at;
        started_at = None;
        completed_at = None;
        notes = None;
        cancelled_by = None;
        cancelled_at = None;
        reason = None;
      }
  | Types.InProgress { assignee; started_at } ->
      {
        status = "in_progress";
        assignee = Some assignee;
        claimed_at = None;
        started_at = Some started_at;
        completed_at = None;
        notes = None;
        cancelled_by = None;
        cancelled_at = None;
        reason = None;
      }
  | Types.Done { assignee; completed_at; notes } ->
      {
        status = "done";
        assignee = Some assignee;
        claimed_at = None;
        started_at = None;
        completed_at = Some completed_at;
        notes;
        cancelled_by = None;
        cancelled_at = None;
        reason = None;
      }
  | Types.Cancelled { cancelled_by; cancelled_at; reason } ->
      {
        status = "cancelled";
        assignee = None;
        claimed_at = None;
        started_at = None;
        completed_at = None;
        notes = None;
        cancelled_by = Some cancelled_by;
        cancelled_at = Some cancelled_at;
        reason;
      }

let page_info_typ =
  Schema.obj "PageInfo"
    ~fields:[
      Schema.field "hasNextPage"
        ~typ:(Schema.non_null Schema.bool)
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.has_next_page);
      Schema.field "endCursor"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.end_cursor);
    ]

let worktree_info_typ =
  Schema.obj "WorktreeInfo"
    ~fields:[
      Schema.field "branch"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (wt : Types.worktree_info) -> wt.branch);
      Schema.field "path"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (wt : Types.worktree_info) -> wt.path);
      Schema.field "gitRoot"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (wt : Types.worktree_info) -> wt.git_root);
      Schema.field "repoName"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (wt : Types.worktree_info) -> wt.repo_name);
    ]

let task_status_typ =
  Schema.obj "TaskStatus"
    ~fields:[
      Schema.field "status"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.status);
      Schema.field "assignee"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.assignee);
      Schema.field "claimedAt"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.claimed_at);
      Schema.field "startedAt"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.started_at);
      Schema.field "completedAt"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.completed_at);
      Schema.field "notes"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.notes);
      Schema.field "cancelledBy"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.cancelled_by);
      Schema.field "cancelledAt"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.cancelled_at);
      Schema.field "reason"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ info -> info.reason);
    ]

let task_typ =
  Schema.obj "Task"
    ~fields:[
      Schema.field "id"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> encode_cursor ~kind:"task" task.id);
      Schema.field "title"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.title);
      Schema.field "description"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.description);
      Schema.field "priority"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.priority);
      Schema.field "files"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null Schema.string)))
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.files);
      Schema.field "createdAt"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.created_at);
      Schema.field "status"
        ~typ:(Schema.non_null task_status_typ)
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task_status_info_of_task task);
      Schema.field "worktree"
        ~typ:worktree_info_typ
        ~args:Arg.[]
        ~resolve:(fun _ (task : Types.task) -> task.worktree);
    ]

let agent_meta_typ =
  Schema.obj "AgentMeta"
    ~fields:[
      Schema.field "sessionId"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.session_id);
      Schema.field "agentType"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.agent_type);
      Schema.field "pid"
        ~typ:Schema.int
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.pid);
      Schema.field "hostname"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.hostname);
      Schema.field "tty"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.tty);
      Schema.field "worktree"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.worktree);
      Schema.field "parentTask"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (meta : Types.agent_meta) -> meta.parent_task);
    ]

let agent_typ =
  Schema.obj "Agent"
    ~fields:[
      Schema.field "id"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> encode_cursor ~kind:"agent" agent.name);
      Schema.field "name"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.name);
      Schema.field "agentType"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.agent_type);
      Schema.field "status"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> Types.agent_status_to_string agent.status);
      Schema.field "capabilities"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null Schema.string)))
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.capabilities);
      Schema.field "currentTask"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.current_task);
      Schema.field "joinedAt"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.joined_at);
      Schema.field "lastSeen"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.last_seen);
      Schema.field "meta"
        ~typ:agent_meta_typ
        ~args:Arg.[]
        ~resolve:(fun _ (agent : Types.agent) -> agent.meta);
    ]

let message_typ =
  Schema.obj "Message"
    ~fields:[
      Schema.field "id"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> encode_cursor ~kind:"message" (string_of_int message.seq));
      Schema.field "seq"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.seq);
      Schema.field "from"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.from_agent);
      Schema.field "messageType"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.msg_type);
      Schema.field "content"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.content);
      Schema.field "mention"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.mention);
      Schema.field "timestamp"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (message : Types.message) -> message.timestamp);
    ]

let room_state_typ =
  Schema.obj "RoomState"
    ~fields:[
      Schema.field "protocolVersion"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.protocol_version);
      Schema.field "project"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.project);
      Schema.field "startedAt"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.started_at);
      Schema.field "messageSeq"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.message_seq);
      Schema.field "activeAgents"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null Schema.string)))
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.active_agents);
      Schema.field "paused"
        ~typ:(Schema.non_null Schema.bool)
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.paused);
      Schema.field "pauseReason"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.pause_reason);
      Schema.field "pausedBy"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.paused_by);
      Schema.field "pausedAt"
        ~typ:Schema.string
        ~args:Arg.[]
        ~resolve:(fun _ (state : Types.room_state) -> state.paused_at);
    ]

let task_edge_typ =
  Schema.obj "TaskEdge"
    ~fields:[
      Schema.field "node"
        ~typ:(Schema.non_null task_typ)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.node);
      Schema.field "cursor"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.cursor);
    ]

let agent_edge_typ =
  Schema.obj "AgentEdge"
    ~fields:[
      Schema.field "node"
        ~typ:(Schema.non_null agent_typ)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.node);
      Schema.field "cursor"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.cursor);
    ]

let message_edge_typ =
  Schema.obj "MessageEdge"
    ~fields:[
      Schema.field "node"
        ~typ:(Schema.non_null message_typ)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.node);
      Schema.field "cursor"
        ~typ:(Schema.non_null Schema.string)
        ~args:Arg.[]
        ~resolve:(fun _ edge -> edge.cursor);
    ]

let task_connection_typ =
  Schema.obj "TaskConnection"
    ~fields:[
      Schema.field "edges"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null task_edge_typ)))
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.edges);
      Schema.field "pageInfo"
        ~typ:(Schema.non_null page_info_typ)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.page_info);
      Schema.field "totalCount"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.total_count);
    ]

let agent_connection_typ =
  Schema.obj "AgentConnection"
    ~fields:[
      Schema.field "edges"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null agent_edge_typ)))
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.edges);
      Schema.field "pageInfo"
        ~typ:(Schema.non_null page_info_typ)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.page_info);
      Schema.field "totalCount"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.total_count);
    ]

let message_connection_typ =
  Schema.obj "MessageConnection"
    ~fields:[
      Schema.field "edges"
        ~typ:(Schema.non_null (Schema.list (Schema.non_null message_edge_typ)))
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.edges);
      Schema.field "pageInfo"
        ~typ:(Schema.non_null page_info_typ)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.page_info);
      Schema.field "totalCount"
        ~typ:(Schema.non_null Schema.int)
        ~args:Arg.[]
        ~resolve:(fun _ conn -> conn.total_count);
    ]

let graphql_error message =
  Yojson.Basic.to_string
    (`Assoc [("errors", `List [`Assoc [("message", `String message)]])])

let rec const_value_of_yojson = function
  | `Null -> (`Null : Graphql_parser.const_value)
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Intlit s ->
      (match int_of_string_opt s with
       | Some i -> `Int i
       | None -> `String s)
  | `Float f -> `Float f
  | `Floatlit s ->
      (match float_of_string_opt s with
       | Some f -> `Float f
       | None -> `String s)
  | `String s -> `String s
  | `Assoc fields ->
      `Assoc (List.map (fun (k, v) -> (k, const_value_of_yojson v)) fields)
  | `List items ->
      `List (List.map const_value_of_yojson items)
  | `Tuple items ->
      `List (List.map const_value_of_yojson items)
  | `Variant (name, None) ->
      `Enum name
  | `Variant (name, Some value) ->
      `Assoc [("type", `Enum name); ("value", const_value_of_yojson value)]

let variables_of_yojson = function
  | None -> []
  | Some (`Null) -> []
  | Some (`Assoc fields) ->
      List.map (fun (k, v) -> (k, const_value_of_yojson v)) fields
  | Some _ -> []

let get_agents config : Types.agent list =
  let dir = Room_utils.agents_dir config in
  Room_utils.list_dir config dir
  |> List.filter (fun name -> Filename.check_suffix name ".json")
  |> List.filter_map (fun name ->
      let path = Filename.concat dir name in
      let json = Room_utils.read_json config path in
      match Types.agent_of_yojson json with
      | Ok agent -> Some agent
      | Error _ -> None)
  |> List.sort (fun (a : Types.agent) (b : Types.agent) -> String.compare a.name b.name)

let get_messages config : Types.message list =
  let dir = Room_utils.messages_dir config in
  Room_utils.list_dir config dir
  |> List.filter Room.is_valid_filename
  |> List.filter (fun name -> Filename.check_suffix name ".json")
  |> List.filter_map (fun name ->
      let path = Filename.concat dir name in
      let json = Room_utils.read_json config path in
      match Types.message_of_yojson json with
      | Ok msg -> Some msg
      | Error _ -> None)
  |> List.sort (fun (a : Types.message) (b : Types.message) -> compare a.seq b.seq)

let tasks_connection config first after =
  let tasks : Types.task list =
    if Room_utils.is_initialized config then
      Room.get_tasks_raw config
    else
      []
  in
  let after_id = Option.bind after (decode_cursor ~kind:"task") in
  let cursor_of (task : Types.task) = encode_cursor ~kind:"task" task.id in
  let items_after = drop_after_id (fun (t : Types.task) -> t.id) tasks after_id in
  let first = clamp_first first in
  let page_items = take first items_after in
  let edges = List.map (fun node -> { node; cursor = cursor_of node }) page_items in
  let has_next_page = List.length items_after > List.length page_items in
  let end_cursor =
    match List.rev edges with
    | [] -> None
    | edge :: _ -> Some edge.cursor
  in
  { edges; page_info = { has_next_page; end_cursor }; total_count = List.length tasks }

let agents_connection config first after =
  let agents : Types.agent list =
    if Room_utils.is_initialized config then
      get_agents config
    else
      []
  in
  let after_id = Option.bind after (decode_cursor ~kind:"agent") in
  let cursor_of (agent : Types.agent) = encode_cursor ~kind:"agent" agent.name in
  let items_after = drop_after_id (fun (a : Types.agent) -> a.name) agents after_id in
  let first = clamp_first first in
  let page_items = take first items_after in
  let edges = List.map (fun node -> { node; cursor = cursor_of node }) page_items in
  let has_next_page = List.length items_after > List.length page_items in
  let end_cursor =
    match List.rev edges with
    | [] -> None
    | edge :: _ -> Some edge.cursor
  in
  { edges; page_info = { has_next_page; end_cursor }; total_count = List.length agents }

let messages_connection config first after =
  let messages : Types.message list =
    if Room_utils.is_initialized config then
      get_messages config
    else
      []
  in
  let after_seq =
    Option.bind after (fun cursor ->
      Option.bind (decode_cursor ~kind:"message" cursor) int_of_string_opt)
  in
  let messages_after =
    match after_seq with
    | None -> messages
    | Some seq -> List.filter (fun msg -> msg.seq > seq) messages
  in
  let first = clamp_first first in
  let page_items = take first messages_after in
  let edges =
    List.map (fun node ->
      { node; cursor = encode_cursor ~kind:"message" (string_of_int node.seq) })
      page_items
  in
  let has_next_page = List.length messages_after > List.length page_items in
  let end_cursor =
    match List.rev edges with
    | [] -> None
    | edge :: _ -> Some edge.cursor
  in
  { edges; page_info = { has_next_page; end_cursor }; total_count = List.length messages }

let schema =
  Schema.schema [
    Schema.field "status"
      ~typ:(Schema.non_null room_state_typ)
      ~args:Arg.[]
      ~resolve:(fun info () -> Room.read_state info.ctx.room_config);
    Schema.field "tasks"
      ~typ:(Schema.non_null task_connection_typ)
      ~args:Arg.[
        Arg.arg "first" ~typ:Arg.int;
        Arg.arg "after" ~typ:Arg.string;
      ]
      ~resolve:(fun info () first after ->
        tasks_connection info.ctx.room_config first after);
    Schema.field "agents"
      ~typ:(Schema.non_null agent_connection_typ)
      ~args:Arg.[
        Arg.arg "first" ~typ:Arg.int;
        Arg.arg "after" ~typ:Arg.string;
      ]
      ~resolve:(fun info () first after ->
        agents_connection info.ctx.room_config first after);
    Schema.field "messages"
      ~typ:(Schema.non_null message_connection_typ)
      ~args:Arg.[
        Arg.arg "first" ~typ:Arg.int;
        Arg.arg "after" ~typ:Arg.string;
      ]
      ~resolve:(fun info () first after ->
        messages_connection info.ctx.room_config first after);
  ]

let handle_request ~config body_str =
  let json = Safe_ops.parse_json_safe ~context:"graphql" body_str in
  match json with
  | Error msg ->
      { status = `Bad_request; body = graphql_error msg }
  | Ok payload ->
      let open Yojson.Safe.Util in
      let query =
        match payload |> member "query" with
        | `String q -> Some q
        | _ -> None
      in
      let variables_json =
        match payload |> member "variables" with
        | `Null -> None
        | other -> Some other
      in
      let operation_name =
        match payload |> member "operationName" with
        | `String s -> Some s
        | _ -> None
      in
      (match query with
       | None ->
           { status = `Bad_request; body = graphql_error "Missing query field" }
       | Some query_str ->
           match Graphql_parser.parse query_str with
           | Error err ->
               { status = `Bad_request; body = graphql_error err }
           | Ok doc ->
               let variables = variables_of_yojson variables_json in
               let ctx = { room_config = config } in
               let result =
                 if variables = [] then
                   Schema.execute schema ctx ?operation_name doc
                 else
                   Schema.execute schema ctx ~variables ?operation_name doc
               in
               match result with
               | Ok (`Response json) ->
                   { status = `OK; body = Yojson.Basic.to_string json }
               | Ok (`Stream _) ->
                   { status = `Bad_request; body = graphql_error "Subscriptions are not supported" }
               | Error err_json ->
                   { status = `OK; body = Yojson.Basic.to_string err_json })
