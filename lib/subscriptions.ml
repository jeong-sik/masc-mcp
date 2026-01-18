(** Resource Subscriptions - MCP 2025-11-25 Spec Compliance

    Implements resource change notifications and subscriptions.
    Clients can subscribe to resources (tasks, agents, etc.) and
    receive updates via SSE.

    MCP Spec MAY: Resource subscriptions for change notifications
*)

(** Subscription types *)
type resource_type =
  | Tasks
  | Agents
  | Messages
  | Votes
  | Custom of string

let resource_type_to_string = function
  | Tasks -> "tasks"
  | Agents -> "agents"
  | Messages -> "messages"
  | Votes -> "votes"
  | Custom s -> s

let resource_type_of_string = function
  | "tasks" -> Tasks
  | "agents" -> Agents
  | "messages" -> Messages
  | "votes" -> Votes
  | s -> Custom s

(** Change type *)
type change_type =
  | Created
  | Updated
  | Deleted

let change_type_to_string = function
  | Created -> "created"
  | Updated -> "updated"
  | Deleted -> "deleted"

(** Subscription record *)
type subscription = {
  id: string;
  subscriber: string;  (* agent_name or session_id *)
  resource: resource_type;
  filter: string option;  (* Optional filter, e.g., task_id *)
  created_at: float;
}

(** Change notification *)
type notification = {
  subscription_id: string;
  resource: resource_type;
  change: change_type;
  resource_id: string;
  data: Yojson.Safe.t;
  timestamp: float;
}

(** Subscription store *)
module SubscriptionStore = struct
  let subscriptions : (string, subscription) Hashtbl.t = Hashtbl.create 64
  let pending_notifications : (string, notification list) Hashtbl.t = Hashtbl.create 64

  (** Generate subscription ID *)
  let generate_id () : string =
    let bytes = Mirage_crypto_rng.generate 8 in
    let buf = Buffer.create 16 in
    for i = 0 to String.length bytes - 1 do
      Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (String.get bytes i)))
    done;
    Printf.sprintf "sub_%s" (Buffer.contents buf)

  (** Subscribe to resource changes *)
  let subscribe ~(subscriber : string) ~(resource : resource_type) ?(filter : string option) () : subscription =
    let sub = {
      id = generate_id ();
      subscriber;
      resource;
      filter;
      created_at = Unix.gettimeofday ();
    } in
    Hashtbl.add subscriptions sub.id sub;
    Log.Sub.info "%s subscribed to %s" subscriber (resource_type_to_string resource);
    sub

  (** Unsubscribe *)
  let unsubscribe (id : string) : bool =
    if Hashtbl.mem subscriptions id then begin
      Hashtbl.remove subscriptions id;
      Hashtbl.remove pending_notifications id;
      true
    end else false

  (** Get subscription by ID *)
  let get (id : string) : subscription option =
    Hashtbl.find_opt subscriptions id

  (** Find subscriptions for a resource change *)
  let find_matching ~(resource : resource_type) ~(resource_id : string) : subscription list =
    Hashtbl.fold (fun _ (sub : subscription) (acc : subscription list) ->
      if sub.resource = resource then
        match sub.filter with
        | None -> sub :: acc  (* No filter = match all *)
        | Some f when f = resource_id -> sub :: acc
        | Some f when f = "*" -> sub :: acc
        | _ -> acc
      else acc
    ) subscriptions []

  (** Get all subscriptions for a subscriber *)
  let get_for_subscriber (subscriber : string) : subscription list =
    Hashtbl.fold (fun _ sub acc ->
      if sub.subscriber = subscriber then sub :: acc else acc
    ) subscriptions []

  (** Queue notification for a subscription *)
  let queue_notification (sub_id : string) (notif : notification) : unit =
    let existing = match Hashtbl.find_opt pending_notifications sub_id with
      | Some l -> l
      | None -> []
    in
    Hashtbl.replace pending_notifications sub_id (existing @ [notif])

  (** Pop notifications for a subscription *)
  let pop_notifications (sub_id : string) : notification list =
    match Hashtbl.find_opt pending_notifications sub_id with
    | Some l ->
      Hashtbl.remove pending_notifications sub_id;
      l
    | None -> []

  (** List all subscriptions *)
  let list_all () : subscription list =
    Hashtbl.fold (fun _ s acc -> s :: acc) subscriptions []

  (** Count subscriptions *)
  let count () : int =
    Hashtbl.length subscriptions
end

(** Notify all subscribers of a resource change *)
let notify_change ~(resource : resource_type) ~(change : change_type) 
    ~(resource_id : string) ~(data : Yojson.Safe.t) : int =
  let subs = SubscriptionStore.find_matching ~resource ~resource_id in
  let now = Unix.gettimeofday () in
  List.iter (fun sub ->
    let notif = {
      subscription_id = sub.id;
      resource;
      change;
      resource_id;
      data;
      timestamp = now;
    } in
    SubscriptionStore.queue_notification sub.id notif
  ) subs;
  List.length subs

(** Subscription to JSON *)
let subscription_to_json (s : subscription) : Yojson.Safe.t =
  `Assoc [
    ("id", `String s.id);
    ("subscriber", `String s.subscriber);
    ("resource", `String (resource_type_to_string s.resource));
    ("filter", match s.filter with None -> `Null | Some f -> `String f);
    ("created_at", `Float s.created_at);
  ]

(** Notification to JSON *)
let notification_to_json (n : notification) : Yojson.Safe.t =
  `Assoc [
    ("subscription_id", `String n.subscription_id);
    ("resource", `String (resource_type_to_string n.resource));
    ("change", `String (change_type_to_string n.change));
    ("resource_id", `String n.resource_id);
    ("data", n.data);
    ("timestamp", `Float n.timestamp);
  ]

(** MCP tool handler for subscriptions *)
let handle_subscription_tool (arguments : Yojson.Safe.t) : (bool * string) =
  let get_string key =
    match Yojson.Safe.Util.member key arguments with
    | `String s -> Some s
    | _ -> None
  in
  match get_string "action" with
  | Some "subscribe" ->
    (match get_string "subscriber", get_string "resource" with
     | Some subscriber, Some resource_str ->
       let resource = resource_type_of_string resource_str in
       let filter = get_string "filter" in
       let sub = SubscriptionStore.subscribe ~subscriber ~resource ?filter () in
       (true, Yojson.Safe.pretty_to_string (subscription_to_json sub))
     | _ -> (false, "subscriber and resource required"))

  | Some "unsubscribe" ->
    (match get_string "subscription_id" with
     | Some id ->
       if SubscriptionStore.unsubscribe id then
         (true, Printf.sprintf "Unsubscribed from '%s'" id)
       else
         (false, Printf.sprintf "Subscription '%s' not found" id)
     | None -> (false, "subscription_id required"))

  | Some "list" ->
    (match get_string "subscriber" with
     | Some subscriber ->
       let subs = SubscriptionStore.get_for_subscriber subscriber in
       let json = `Assoc [
         ("count", `Int (List.length subs));
         ("subscriptions", `List (List.map subscription_to_json subs));
       ] in
       (true, Yojson.Safe.pretty_to_string json)
     | None ->
       let subs = SubscriptionStore.list_all () in
       let json = `Assoc [
         ("count", `Int (List.length subs));
         ("subscriptions", `List (List.map subscription_to_json subs));
       ] in
       (true, Yojson.Safe.pretty_to_string json))

  | Some "poll" ->
    (match get_string "subscription_id" with
     | Some id ->
       let notifs = SubscriptionStore.pop_notifications id in
       let json = `Assoc [
         ("count", `Int (List.length notifs));
         ("notifications", `List (List.map notification_to_json notifs));
       ] in
       (true, Yojson.Safe.pretty_to_string json)
     | None -> (false, "subscription_id required"))

  | Some other -> (false, Printf.sprintf "Unknown action: %s" other)
  | None -> (false, "action required: subscribe, unsubscribe, list, poll")

(** Hook function to notify task changes - call from Room module *)
let notify_task_change ~(change : change_type) ~(task_id : string) ~(data : Yojson.Safe.t) : unit =
  let _ = notify_change ~resource:Tasks ~change ~resource_id:task_id ~data in
  ()

(** Hook function to notify agent changes *)
let notify_agent_change ~(change : change_type) ~(agent_name : string) ~(data : Yojson.Safe.t) : unit =
  let _ = notify_change ~resource:Agents ~change ~resource_id:agent_name ~data in
  ()

(** Hook function to notify message changes *)
let notify_message ~(message_id : string) ~(data : Yojson.Safe.t) : unit =
  let _ = notify_change ~resource:Messages ~change:Created ~resource_id:message_id ~data in
  ()
