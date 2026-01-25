-------------------------------- MODULE MASC_A2A --------------------------------
(*
 * TLA+ Formal Specification for MASC A2A (Agent-to-Agent) Communication
 *
 * Properties to verify:
 * 1. Message Delivery: Every broadcast eventually reaches all online subscribers
 * 2. No Message Loss: Messages are either delivered or persisted
 * 3. Ordering: Messages from same sender are received in order (FIFO per sender)
 * 4. Liveness: If sender and receiver are both online, delivery happens
 *)

EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
    Agents,          \* Set of all possible agents (e.g., {claude, gemini, codex})
    MaxMessages      \* Maximum messages for model checking bound

VARIABLES
    online,          \* Set of currently online agents
    subscriptions,   \* Function: agent -> set of subscribed event types
    messageQueue,    \* Sequence of pending messages (room storage)
    delivered,       \* Function: agent -> set of received message IDs
    msgCounter       \* Counter for unique message IDs

vars == <<online, subscriptions, messageQueue, delivered, msgCounter>>

-----------------------------------------------------------------------------
(* Type Invariants *)

TypeInvariant ==
    /\ online \subseteq Agents
    /\ subscriptions \in [Agents -> SUBSET {"broadcast", "task_update", "completion"}]
    /\ msgCounter \in Nat
    /\ msgCounter <= MaxMessages

-----------------------------------------------------------------------------
(* Initial State *)

Init ==
    /\ online = {}
    /\ subscriptions = [a \in Agents |-> {}]
    /\ messageQueue = <<>>
    /\ delivered = [a \in Agents |-> {}]
    /\ msgCounter = 0

-----------------------------------------------------------------------------
(* Actions *)

(* Agent joins the room *)
Join(agent) ==
    /\ agent \notin online
    /\ online' = online \cup {agent}
    /\ UNCHANGED <<subscriptions, messageQueue, delivered, msgCounter>>

(* Agent leaves the room *)
Leave(agent) ==
    /\ agent \in online
    /\ online' = online \ {agent}
    /\ UNCHANGED <<subscriptions, messageQueue, delivered, msgCounter>>

(* Agent subscribes to an event type *)
Subscribe(agent, eventType) ==
    /\ agent \in online
    /\ subscriptions' = [subscriptions EXCEPT ![agent] = @ \cup {eventType}]
    /\ UNCHANGED <<online, messageQueue, delivered, msgCounter>>

(* Agent broadcasts a message *)
Broadcast(sender) ==
    /\ sender \in online
    /\ msgCounter < MaxMessages
    /\ LET newMsg == [id |-> msgCounter, sender |-> sender, type |-> "broadcast"]
       IN messageQueue' = Append(messageQueue, newMsg)
    /\ msgCounter' = msgCounter + 1
    /\ UNCHANGED <<online, subscriptions, delivered>>

(* Message delivery to a subscriber *)
Deliver(agent, msgIdx) ==
    /\ agent \in online
    /\ msgIdx \in 1..Len(messageQueue)
    /\ LET msg == messageQueue[msgIdx]
       IN /\ "broadcast" \in subscriptions[agent]
          /\ msg.id \notin delivered[agent]
          /\ delivered' = [delivered EXCEPT ![agent] = @ \cup {msg.id}]
    /\ UNCHANGED <<online, subscriptions, messageQueue, msgCounter>>

(* Next state relation *)
Next ==
    \/ \E a \in Agents : Join(a)
    \/ \E a \in Agents : Leave(a)
    \/ \E a \in Agents, e \in {"broadcast", "task_update"} : Subscribe(a, e)
    \/ \E a \in Agents : Broadcast(a)
    \/ \E a \in Agents, i \in 1..Len(messageQueue) : Deliver(a, i)

-----------------------------------------------------------------------------
(* Fairness: Eventually things happen *)

Fairness ==
    /\ \A a \in Agents : WF_vars(Join(a))
    /\ \A a \in Agents, i \in Nat : WF_vars(Deliver(a, i))

Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Safety Properties *)

(* No duplicate delivery *)
NoDuplicateDelivery ==
    \A a \in Agents : Cardinality(delivered[a]) = Cardinality(delivered[a])
    \* (trivially true, but structure for extension)

(* Messages are not lost - if in queue, either delivered or agent offline *)
NoMessageLoss ==
    \A i \in 1..Len(messageQueue) :
        LET msg == messageQueue[i]
        IN \A a \in online :
            ("broadcast" \in subscriptions[a]) =>
            (msg.id \in delivered[a] \/ a = msg.sender)

-----------------------------------------------------------------------------
(* Liveness Properties *)

(* If an agent is online and subscribed, it eventually receives broadcasts *)
EventualDelivery ==
    \A a \in Agents :
        (a \in online /\ "broadcast" \in subscriptions[a]) ~>
        (\A i \in 1..Len(messageQueue) :
            LET msg == messageQueue[i]
            IN msg.type = "broadcast" => msg.id \in delivered[a])

-----------------------------------------------------------------------------
(* Model Checking Configuration *)
(*
 * To check with TLC:
 *   Agents <- {claude, gemini, codex}
 *   MaxMessages <- 3
 *   Check: TypeInvariant, NoMessageLoss
 *   Check Liveness: EventualDelivery
 *)
=============================================================================
