# MASC Research-Based Improvements

**Date**: 2026-01-09
**Based on**: Expert Panel Review + Academic Literature Survey
**Status**: Proposal

---

## Research Sources

이 문서의 개선안은 다음 학술 연구에 기반합니다:

| ID | Title | Venue | Year |
|----|-------|-------|------|
| R1 | [Multi-Agent Collaboration Mechanisms: A Survey](https://arxiv.org/abs/2501.06322) | arXiv | 2025 |
| R2 | [LLM-Based Multi-Agent Systems for Software Engineering](https://dl.acm.org/doi/10.1145/3712003) | ACM TOSEM | 2024 |
| R3 | [EvoAgent: Automatic Multi-Agent Generation](https://arxiv.org/abs/2406.14228) | arXiv | 2024 |
| R4 | [EC-MAS Confluence Survey](https://www.ieee-jas.net/en/article/doi/10.1109/JAS.2025.125246) | IEEE JAS | 2025 |
| R5 | [Hippocampus-Inspired Stability-Plasticity](https://pmc.ncbi.nlm.nih.gov/articles/PMC11591613/) | PMC | 2024 |
| R6 | [Neural Reshaping: Plasticity in AI](https://pmc.ncbi.nlm.nih.gov/articles/PMC11751442/) | PMC | 2024 |
| R7 | [Effective Programming: OCaml Effects](https://www.janestreet.com/tech-talks/effective-programming/) | Jane Street | 2024 |
| R8 | [Retrofitting Effect Handlers onto OCaml](https://dl.acm.org/doi/10.1145/3453483.3454039) | ACM PLDI | 2021 |
| R9 | [Modified Evolutionary RL for MAS](https://link.springer.com/article/10.1007/s40747-024-01385-4) | Springer | 2024 |
| R10 | [Auto-scaling LLM-based MAS](https://www.frontiersin.org/journals/artificial-intelligence/articles/10.3389/frai.2025.1638227/full) | Frontiers | 2025 |
| R11 | [Active Objects with Algebraic Effects](https://link.springer.com/chapter/10.1007/978-3-031-51060-1_1) | Springer | 2024 |

---

## Priority Matrix

| Priority | Improvement | Research | Expert | Impact |
|----------|-------------|----------|--------|--------|
| 🔴 P0 | Error Propagation Guard | R1 | Cold Critic | Knowledge drift 방지 |
| 🔴 P0 | Fitness Selection | R3, R4, R9 | Darwin | 최적 에이전트 선택 |
| 🟠 P1 | Hebbian Learning | R5, R6 | Neuroscientist | 협업 패턴 최적화 |
| 🟠 P1 | Effect System | R7, R8, R11 | Haskell Master | 타입 안전성 극대화 |
| 🟡 P2 | Terminology Normalization | R2 | Chomsky | 코드 가독성 |
| 🟡 P2 | Telemetry/Analytics | R2, R10 | YC | Data-driven 의사결정 |
| 🟢 P3 | KV-Cache Transfer | R1 | Musk | Handoff 지연 최소화 |

---

## 🔴 P0: Error Propagation Guard

### 문제 (R1)

> "Knowledge drift leads to amplification and propagation of errors through agent chains. Unlike humans who naturally filter information, LLMs exhibit cognitive bias expansion."

### 구현

```ocaml
(* lib/drift_guard.ml *)

type verification_result =
  | Verified of context
  | Drift_detected of {
      original: context;
      received: context;
      similarity: float;
      drift_type: [`Semantic | `Factual | `Structural]
    }

(** Verify context integrity during handoff *)
let verify_handoff ~source ~target : verification_result =
  let similarity = Semantic.cosine_similarity
    source.summary
    target.received_summary
  in
  if similarity < 0.85 then
    Drift_detected {
      original = source;
      received = target;
      similarity;
      drift_type = classify_drift source target
    }
  else
    Verified target

(** Auto-correction on drift detection *)
let handle_drift = function
  | Verified ctx -> Lwt.return ctx
  | Drift_detected d ->
      Log.warn "Drift detected: %.2f similarity" d.similarity;
      (* Re-request from source or use original *)
      match d.drift_type with
      | `Semantic -> correct_semantic_drift d
      | `Factual -> request_clarification d
      | `Structural -> reformat_context d
```

### 테스트 케이스

```ocaml
let%test "drift detection" =
  let original = { summary = "Implement user auth with JWT" } in
  let drifted = { summary = "Implement user auth with session cookies" } in
  match verify_handoff ~source:original ~target:drifted with
  | Drift_detected d -> d.similarity < 0.85
  | Verified _ -> false
```

---

## 🔴 P0: Fitness Selection

### 문제 (R3, R4)

> "EvoAgent applies evolutionary operators (mutation, crossover, selection) to automatically extend specialized agents." - R3
> "Fine-grained agent-based EC treats each individual in the population as an agent with fitness-driven selection." - R4

### 구현

```ocaml
(* lib/fitness.ml *)

type metrics = {
  task_completion_rate: float;    (** 0.0 - 1.0 *)
  avg_response_time: float;       (** seconds *)
  error_rate: float;              (** 0.0 - 1.0 *)
  handoff_success_rate: float;    (** 0.0 - 1.0 *)
  collaboration_score: float;     (** Hebbian-derived, 0.0 - 1.0 *)
}

type fitness_score = float  (** Weighted combination, 0.0 - 1.0 *)

(** Calculate fitness from metrics *)
let calculate_fitness (m : metrics) : fitness_score =
  let weights = {
    completion = 0.35;
    speed = 0.15;
    reliability = 0.25;
    handoff = 0.15;
    collaboration = 0.10;
  } in
  (m.task_completion_rate *. weights.completion) +.
  ((1.0 -. min 1.0 (m.avg_response_time /. 60.0)) *. weights.speed) +.
  ((1.0 -. m.error_rate) *. weights.reliability) +.
  (m.handoff_success_rate *. weights.handoff) +.
  (m.collaboration_score *. weights.collaboration)

(** Roulette wheel selection - R9 inspired *)
let select_agent ~task ~available_agents =
  let scored = List.map (fun agent ->
    let metrics = Metrics.get_recent agent.id ~window:(Days 7) in
    (agent, calculate_fitness metrics)
  ) available_agents in

  let total = List.fold_left (fun acc (_, s) -> acc +. s) 0.0 scored in
  let threshold = Random.float total in
  roulette_select scored threshold

(** Elite selection for critical tasks *)
let select_elite ~task ~agents ~top_n =
  let scored = List.map (fun a -> (a, calculate_fitness (Metrics.get a.id))) agents in
  let sorted = List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored in
  List.take top_n sorted |> List.map fst
```

### 데이터 스키마

```sql
-- PostgreSQL or SQLite
CREATE TABLE agent_metrics (
  id SERIAL PRIMARY KEY,
  agent_id TEXT NOT NULL,
  task_id TEXT,
  completion_success BOOLEAN,
  response_time_ms INTEGER,
  error_occurred BOOLEAN,
  handoff_success BOOLEAN,
  recorded_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_agent_metrics_agent ON agent_metrics(agent_id, recorded_at);
```

---

## 🟠 P1: Hebbian Learning

### 문제 (R5, R6)

> "Dual learning rates, offline consolidation, and dynamic plasticity modulation." - R5
> "Machine learning models, similar to human neuroplasticity, enhance performance through iterative learning." - R6

### 구현

```ocaml
(* lib/hebbian.ml *)

type synapse = {
  from_agent: string;
  to_agent: string;
  weight: float;        (** 0.0 - 1.0, collaboration strength *)
  last_updated: float;  (** Unix timestamp *)
}

type learning_rate = {
  fast: float;  (** Immediate, session-level: 0.1 *)
  slow: float;  (** Consolidated, daily: 0.01 *)
}

let default_rate = { fast = 0.1; slow = 0.01 }

(** Strengthen connection on successful collaboration *)
let strengthen ~from_agent ~to_agent =
  let current = get_synapse from_agent to_agent in
  let delta = default_rate.fast *. (1.0 -. current.weight) in
  set_synapse { current with
    weight = min 1.0 (current.weight +. delta);
    last_updated = Unix.gettimeofday ()
  }

(** Weaken connection on failed collaboration *)
let weaken ~from_agent ~to_agent =
  let current = get_synapse from_agent to_agent in
  let delta = default_rate.fast *. current.weight in
  set_synapse { current with
    weight = max 0.0 (current.weight -. delta);
    last_updated = Unix.gettimeofday ()
  }

(** Get preferred collaboration partner *)
let get_preferred_partner agent_id =
  let synapses = get_all_synapses_from agent_id in
  let sorted = List.sort (fun s1 s2 -> compare s2.weight s1.weight) synapses in
  match sorted with
  | [] -> None
  | strongest :: _ -> Some strongest.to_agent

(** Offline consolidation - run nightly *)
let consolidate () =
  let all_synapses = get_all_synapses () in
  List.iter (fun s ->
    (* Decay unused connections *)
    let days_since_update = (Unix.gettimeofday () -. s.last_updated) /. 86400.0 in
    if days_since_update > 7.0 then
      let decay = default_rate.slow *. days_since_update in
      set_synapse { s with weight = max 0.0 (s.weight -. decay) }
  ) all_synapses
```

### 사용 예시

```ocaml
(* Task completion hook *)
let on_task_complete ~agents ~success =
  let pairs = List.combinations 2 agents in
  List.iter (fun [a; b] ->
    if success then begin
      Hebbian.strengthen ~from_agent:a.id ~to_agent:b.id;
      Hebbian.strengthen ~from_agent:b.id ~to_agent:a.id
    end else begin
      Hebbian.weaken ~from_agent:a.id ~to_agent:b.id;
      Hebbian.weaken ~from_agent:b.id ~to_agent:a.id
    end
  ) pairs
```

---

## 🟠 P1: Effect System (OCaml 5.3+)

### 문제 (R7, R8)

> "By extending OCaml's type system with algebraic effects, side effects become explicitly tracked in types." - R7

### 구현

```ocaml
(* lib/effects.ml - OCaml 5.3+ *)

(* Effect declarations *)
effect Read_file : string -> string
effect Write_file : string * string -> unit
effect Get_time : unit -> float
effect Log : string -> unit
effect Http_request : string * string -> string

(* Pure business logic - no effects *)
module Pure = struct
  let calculate_threshold ~messages ~tool_calls =
    let base = 0.7 in
    let msg_factor = float_of_int messages *. 0.001 in
    let tool_factor = float_of_int tool_calls *. 0.005 in
    min 1.0 (base +. msg_factor +. tool_factor)

  let should_relay ~usage ~threshold =
    usage > threshold

  let compress_context ~context ~max_tokens =
    if String.length context < max_tokens then context
    else String.sub context 0 max_tokens
end

(* Effectful operations - tracked in types *)
module Effectful = struct
  let read_agent_state agent_id =
    let path = Printf.sprintf ".masc/agents/%s.json" agent_id in
    perform (Read_file path)

  let save_task task =
    let path = Printf.sprintf ".masc/tasks/%s.json" task.id in
    let content = Task.to_json task in
    perform (Write_file (path, content))

  let broadcast message =
    perform (Log (Printf.sprintf "Broadcasting: %s" message))
end

(* Handler for production *)
let run_with_filesystem computation =
  match computation () with
  | result -> result
  | effect (Read_file path) k ->
      let content = In_channel.read_all path in
      continue k content
  | effect (Write_file (path, content)) k ->
      Out_channel.write_all path ~data:content;
      continue k ()
  | effect (Get_time ()) k ->
      continue k (Unix.gettimeofday ())
  | effect (Log msg) k ->
      Printf.eprintf "[LOG] %s\n" msg;
      continue k ()

(* Handler for testing - no real I/O *)
let run_with_mock ~mock_fs computation =
  match computation () with
  | result -> result
  | effect (Read_file path) k ->
      let content = Hashtbl.find_opt mock_fs path |> Option.value ~default:"" in
      continue k content
  | effect (Write_file (path, content)) k ->
      Hashtbl.replace mock_fs path content;
      continue k ()
  | effect (Get_time ()) k ->
      continue k 1704067200.0  (* Fixed time for deterministic tests *)
  | effect (Log _) k ->
      continue k ()
```

---

## 🟡 P2: Terminology Normalization

### 문제 (R2)

> "Orchestration platform serves as the core infrastructure that manages interactions and information flow among agents."

### 현재 불일치

| 개념 | 현재 용어들 | 통일안 |
|------|-------------|--------|
| 컨텍스트 전달 | Relay, Handover, Mitosis | **Handoff** |
| 압축된 컨텍스트 | DNA, Summary, Context | **Capsule** |
| 에이전트 생명주기 | Cell state, Lifecycle | **Lifecycle** |
| 작업 단위 | Task, Job, Work | **Task** |

### 마이그레이션 계획

```bash
# 1. 코드 내 용어 변경
sed -i 's/relay_/handoff_/g' lib/*.ml
sed -i 's/dna_/capsule_/g' lib/*.ml

# 2. 타입 별칭 추가 (점진적 마이그레이션)
type handoff = relay  (* Deprecated *)
type capsule = dna    (* Deprecated *)

# 3. 문서 업데이트
# GLOSSARY.md 생성
```

---

## 🟡 P2: Telemetry/Analytics

### 문제 (R2, R10)

> "The orchestration platform facilitates coordination, communication, planning, and learning." - R2

### 구현

```ocaml
(* lib/telemetry.ml *)

type event =
  | Agent_joined of { agent_id: string; capabilities: string list }
  | Agent_left of { agent_id: string; reason: string }
  | Task_started of { task_id: string; agent_id: string }
  | Task_completed of { task_id: string; duration_ms: int; success: bool }
  | Handoff_triggered of { from_agent: string; to_agent: string; reason: string }
  | Error_occurred of { code: string; message: string; context: string }

type metrics = {
  active_agents: int;
  tasks_in_progress: int;
  tasks_completed_24h: int;
  avg_task_duration_ms: float;
  handoff_rate: float;
  error_rate: float;
}

let track event =
  let json = event_to_json event in
  (* Local file + optional remote *)
  Lwt.async (fun () ->
    let%lwt () = append_to_file ".masc/telemetry.jsonl" json in
    match Sys.getenv_opt "MASC_TELEMETRY_URL" with
    | Some url -> Http.post url json
    | None -> Lwt.return_unit
  )

let get_metrics () : metrics =
  let events = read_events ~since:(Hours 24) in
  {
    active_agents = count_active_agents events;
    tasks_in_progress = count_tasks_in_progress events;
    tasks_completed_24h = count_completed_tasks events;
    avg_task_duration_ms = avg_duration events;
    handoff_rate = calculate_handoff_rate events;
    error_rate = calculate_error_rate events;
  }

let export_prometheus () =
  let m = get_metrics () in
  Printf.sprintf {|
# HELP masc_active_agents Number of active agents
# TYPE masc_active_agents gauge
masc_active_agents %d

# HELP masc_tasks_completed Tasks completed in last 24h
# TYPE masc_tasks_completed counter
masc_tasks_completed %d

# HELP masc_avg_task_duration Average task duration in ms
# TYPE masc_avg_task_duration gauge
masc_avg_task_duration %.2f

# HELP masc_handoff_rate Handoff rate (0-1)
# TYPE masc_handoff_rate gauge
masc_handoff_rate %.4f

# HELP masc_error_rate Error rate (0-1)
# TYPE masc_error_rate gauge
masc_error_rate %.4f
|} m.active_agents m.tasks_completed_24h m.avg_task_duration_ms m.handoff_rate m.error_rate
```

---

## 🟢 P3: KV-Cache Transfer

### 문제 (R1)

> "Cache-to-Cache (C2C) proposes direct semantic communication between LLMs using their internal KV-cache, bypassing inefficient text generation."

### 연구 단계

이 기능은 LLM 내부 구조에 접근이 필요하여 현재 구현 불가능합니다. 향후 API 지원 시 구현 예정.

```ocaml
(* lib/kv_cache.ml - Future implementation *)

(** KV-cache snapshot for direct transfer *)
type kv_cache = {
  model: string;
  layer_count: int;
  key_cache: bytes array;
  value_cache: bytes array;
  sequence_length: int;
}

(** Extract cache from running agent - requires API support *)
let extract_cache ~agent_id : kv_cache option Lwt.t =
  (* TODO: Requires LLM provider API support *)
  (* Claude/Gemini don't expose KV-cache yet *)
  Lwt.return None

(** Inject cache into new agent - requires API support *)
let inject_cache ~agent_id ~cache : bool Lwt.t =
  (* TODO: Requires LLM provider API support *)
  Lwt.return false
```

---

## Implementation Roadmap

### Phase 1 (Week 1-2): Foundation
- [ ] Fitness module implementation
- [ ] Basic telemetry
- [ ] Terminology normalization (aliases)

### Phase 2 (Week 3-4): Learning
- [ ] Hebbian synapse tracking
- [ ] Drift guard implementation
- [ ] Metrics dashboard

### Phase 3 (Week 5-6): Type Safety
- [ ] Effect system migration (OCaml 5.3+)
- [ ] Test handler injection
- [ ] Pure/Effectful separation

### Phase 4 (Ongoing): Advanced
- [ ] KV-cache transfer research
- [ ] A2A protocol integration
- [ ] Cross-room federation

---

## References

All citations are stored in Neo4j Knowledge Graph:

```cypher
MATCH (r:Research {id: "research_20260109_161421"})-[:CITES]->(c:Citation)
RETURN c.title, c.url, c.key_insight
```
