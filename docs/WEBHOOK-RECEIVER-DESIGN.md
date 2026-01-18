# MASC Webhook Receiver 설계

> "Webhook 들어오면 MASC Room 중 하나에 쏴준다"

## 1. 핵심 아이디어

```
외부 이벤트 ──► MASC Webhook ──► Room으로 Task 생성 ──► Agent가 Claim
```

**기존 설계와 차이점:**

| 기존 (webhook-gateway) | 새 설계 (MASC 통합) |
|------------------------|---------------------|
| Webhook → 직접 LLM spawn | Webhook → Room Task |
| 단일 실행 | 협업 가능 |
| Hardcoded 라우팅 | Room 기반 유연한 라우팅 |

---

## 2. 아키텍처

```
┌─────────────────────────────────────────────────────────────────┐
│                         MASC Server                             │
│                                                                 │
│   ┌──────────────┐    ┌──────────────┐    ┌──────────────────┐ │
│   │  HTTP Server │    │   Webhook    │    │     Rooms        │ │
│   │  (Eio)       │───►│   Handler    │───►│  ┌────────────┐  │ │
│   │              │    │              │    │  │ pr-review  │  │ │
│   │ /webhook/*   │    │ - Parse      │    │  │ - claude   │  │ │
│   │ /health      │    │ - Verify     │    │  │ - gemini   │  │ │
│   │ /sse         │    │ - Route      │    │  └────────────┘  │ │
│   └──────────────┘    └──────────────┘    │  ┌────────────┐  │ │
│                                           │  │ jira-work  │  │ │
│                                           │  │ - codex    │  │ │
│                                           │  └────────────┘  │ │
│                                           └──────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

---

## 3. Webhook Endpoints

### 3.1 Endpoint 목록

| Endpoint | Source | 설명 |
|----------|--------|------|
| `POST /webhook/github` | GitHub | PR, Issue, Push, Comment |
| `POST /webhook/slack` | Slack | app_mention, message |
| `POST /webhook/jira` | JIRA | issue_created, updated |
| `POST /webhook/generic` | Any | 범용 JSON payload |

### 3.2 Endpoint 정의

```ocaml
(* http_server_eio.ml에 추가 *)

let webhook_routes = [
  ("/webhook/github", handle_github_webhook);
  ("/webhook/slack", handle_slack_webhook);
  ("/webhook/jira", handle_jira_webhook);
  ("/webhook/generic", handle_generic_webhook);
]
```

---

## 4. Webhook → Room 라우팅

### 4.1 라우팅 설정 (`.masc/webhook_routes.json`)

```json
{
  "routes": [
    {
      "source": "github",
      "event": "pull_request.opened",
      "target_room": "pr-review",
      "priority": 2
    },
    {
      "source": "github",
      "event": "issues.labeled",
      "filter": { "label": "AI-WIP" },
      "target_room": "auto-code",
      "priority": 1
    },
    {
      "source": "slack",
      "event": "app_mention",
      "target_room": "slack-support",
      "priority": 3
    },
    {
      "source": "jira",
      "event": "issue_created",
      "target_room": "jira-work",
      "priority": 3
    }
  ],
  "default_room": "general"
}
```

### 4.2 라우팅 로직

```ocaml
(* webhook_router.ml *)

type route = {
  source: string;           (* github | slack | jira *)
  event: string;            (* pull_request.opened 등 *)
  filter: Yojson.Safe.t option;
  target_room: string;
  priority: int;
}

let find_route ~source ~event ~payload routes =
  routes
  |> List.filter (fun r -> r.source = source && matches_event r.event event)
  |> List.filter (fun r -> matches_filter r.filter payload)
  |> List.sort (fun a b -> compare a.priority b.priority)
  |> List.hd_opt
```

---

## 5. Task 생성

### 5.1 Webhook → Task 변환

```ocaml
(* webhook_handler.ml *)

let webhook_to_task ~route ~payload ~raw_body =
  let task_id = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
  {
    id = task_id;
    title = extract_title route.source payload;
    description = extract_description route.source payload;
    priority = route.priority;
    status = "pending";
    room = route.target_room;
    source = route.source;
    source_event = extract_event route.source payload;
    source_url = extract_url route.source payload;
    payload = payload;           (* 파싱된 JSON *)
    raw_body = Some raw_body;    (* 원본 (서명 검증용) *)
    created_at = Unix.gettimeofday ();
    claimed_by = None;
  }
```

### 5.2 이벤트별 제목 추출

```ocaml
let extract_title source payload =
  match source with
  | "github" ->
      let action = payload |> member "action" |> to_string in
      let pr = payload |> member "pull_request" in
      if pr <> `Null then
        Printf.sprintf "[PR] %s #%d: %s"
          action
          (pr |> member "number" |> to_int)
          (pr |> member "title" |> to_string)
      else
        let issue = payload |> member "issue" in
        Printf.sprintf "[Issue] %s #%d: %s"
          action
          (issue |> member "number" |> to_int)
          (issue |> member "title" |> to_string)
  | "slack" ->
      let text = payload |> member "event" |> member "text" |> to_string in
      Printf.sprintf "[Slack] %s" (String.sub text 0 (min 50 (String.length text)))
  | "jira" ->
      let issue = payload |> member "issue" in
      Printf.sprintf "[JIRA] %s: %s"
        (issue |> member "key" |> to_string)
        (issue |> member "fields" |> member "summary" |> to_string)
  | _ -> "[Webhook] New event"
```

---

## 6. 서명 검증

### 6.1 환경변수

```bash
GITHUB_WEBHOOK_SECRET=xxx
SLACK_SIGNING_SECRET=xxx
JIRA_WEBHOOK_SECRET=xxx
```

### 6.2 검증 로직

```ocaml
(* signature.ml *)

(** HMAC-SHA256 서명 검증 *)
let verify_github_signature ~secret ~signature ~body =
  let expected =
    "sha256=" ^
    (Digestif.SHA256.hmac_string ~key:secret body
     |> Digestif.SHA256.to_hex)
  in
  secure_compare expected signature

(** Slack 서명 검증 (v0 format) *)
let verify_slack_signature ~secret ~signature ~timestamp ~body =
  let base = Printf.sprintf "v0:%s:%s" timestamp body in
  let expected =
    "v0=" ^
    (Digestif.SHA256.hmac_string ~key:secret base
     |> Digestif.SHA256.to_hex)
  in
  secure_compare expected signature

(** 타이밍 공격 방지 비교 *)
let secure_compare a b =
  if String.length a <> String.length b then false
  else
    let result = ref 0 in
    for i = 0 to String.length a - 1 do
      result := !result lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !result = 0
```

---

## 7. Room으로 Task 전달

### 7.1 기존 Room API 활용

```ocaml
(* Room에 task 추가 - 기존 room.ml 활용 *)
let dispatch_to_room ~room_name task =
  (* 1. Room 존재 확인 *)
  let room = Room.get room_name in
  match room with
  | None ->
      (* Room 없으면 생성하거나 default room 사용 *)
      let default = Room.get "general" in
      Room.add_task default task
  | Some r ->
      Room.add_task r task

(* 2. Room에 있는 agent들에게 알림 *)
let notify_room ~room_name ~task =
  Broadcast.send ~room:room_name
    (Printf.sprintf "📬 New task: %s (priority: %d)" task.title task.priority)
```

### 7.2 Agent의 Task 처리 흐름

```
1. Agent가 Room에 join
   → masc_join --room pr-review --capabilities "typescript,review"

2. Webhook 도착 → Room에 Task 생성
   → broadcast: "📬 New task: [PR] Review #42"

3. Agent가 task 확인
   → masc_task_list --room pr-review --status pending

4. Agent가 claim
   → masc_claim --task-id xxx

5. Agent가 작업 수행 후 완료
   → masc_done --task-id xxx --result "approved"
```

---

## 8. 새 MCP Tool

### 8.1 Webhook 관련 Tools

```ocaml
(* tools.ml에 추가 *)

(** 웹훅 라우팅 설정 조회 *)
let masc_webhook_routes () =
  Webhook_router.list_routes ()

(** 웹훅 라우팅 추가 *)
let masc_webhook_route_add ~source ~event ~target_room ?filter ?priority () =
  Webhook_router.add_route { source; event; target_room; filter; priority }

(** 웹훅 라우팅 삭제 *)
let masc_webhook_route_remove ~source ~event =
  Webhook_router.remove_route ~source ~event

(** 웹훅 히스토리 조회 *)
let masc_webhook_history ?room ?source ?limit () =
  Webhook_log.list ?room ?source ?limit ()
```

### 8.2 기존 Task Tools 확장

```ocaml
(* 이미 있는 tools를 webhook과 연동 *)

(** Task 목록 - room 필터 추가 *)
let masc_task_list ?room ?status ?source () = ...

(** Task 상세 - webhook payload 포함 *)
let masc_task_get ~task_id () =
  (* payload, source_url 등 webhook 정보 포함 *)
  ...
```

---

## 9. 파일 구조

```
features/masc-mcp/lib/
├── webhook/                    # NEW: Webhook 모듈
│   ├── webhook_types.ml        # Webhook 타입 정의
│   ├── webhook_signature.ml    # 서명 검증
│   ├── webhook_parser.ml       # 이벤트 파싱
│   ├── webhook_router.ml       # Room 라우팅
│   └── webhook_handler.ml      # 메인 핸들러
├── http_server_eio.ml          # MODIFIED: /webhook/* 추가
├── room_eio.ml                 # 기존: Room 관리
├── tools.ml                    # MODIFIED: webhook tools 추가
└── types.ml                    # MODIFIED: webhook task 타입
```

---

## 10. 설정 파일

### `.masc/webhook_routes.json`

```json
{
  "routes": [
    {
      "source": "github",
      "event": "pull_request.*",
      "target_room": "pr-review",
      "priority": 2
    }
  ],
  "secrets": {
    "github": "${GITHUB_WEBHOOK_SECRET}",
    "slack": "${SLACK_SIGNING_SECRET}",
    "jira": "${JIRA_WEBHOOK_SECRET}"
  },
  "default_room": "general",
  "log_retention_days": 7
}
```

---

## 11. 구현 순서

### Phase 1: 최소 기능 (1-2일)

- [ ] `webhook_types.ml` - 기본 타입
- [ ] `webhook_handler.ml` - GitHub PR 이벤트만
- [ ] `http_server_eio.ml` - `/webhook/github` 추가
- [ ] Task 생성 → Room 전달

### Phase 2: 서명 검증 (1일)

- [ ] `webhook_signature.ml` - HMAC 검증
- [ ] 환경변수 로드

### Phase 3: 라우팅 설정 (1일)

- [ ] `webhook_router.ml` - JSON 설정 파일
- [ ] `webhook_routes.json` 포맷

### Phase 4: 확장 (2일)

- [ ] Slack 웹훅
- [ ] JIRA 웹훅
- [ ] MCP Tools 추가

---

## 12. 테스트 계획

```bash
# 1. 로컬 테스트
dune exec masc-mcp -- --http --port 8935

# 2. GitHub webhook 시뮬레이션
curl -X POST http://localhost:8935/webhook/github \
  -H "Content-Type: application/json" \
  -H "X-GitHub-Event: pull_request" \
  -d '{
    "action": "opened",
    "pull_request": {"number": 42, "title": "Test PR"},
    "repository": {"full_name": "owner/repo"}
  }'

# 3. Room에 task 생성 확인
curl http://localhost:8935/health
# → tasks in rooms 확인

# 4. Agent가 claim
masc_claim --task-id <생성된 task id>
```

---

## 13. Open Questions

1. **Room 자동 생성?**
   - 타겟 room이 없을 때 자동 생성할지, 에러 반환할지?
   - 제안: default_room 사용 + warning 로그

2. **Idempotency**
   - 같은 webhook 두 번 오면?
   - 제안: `X-GitHub-Delivery` 헤더로 중복 체크

3. **Rate Limiting**
   - GitHub burst로 많이 올 때?
   - 제안: Phase 3에서 처리 (일단은 없이 시작)

4. **Agent 없는 Room**
   - Task만 쌓이고 아무도 안 가져가면?
   - 제안: TTL 설정 + 만료 시 알림

---

## Changelog

| 날짜 | 변경 |
|------|------|
| 2026-01-13 | 초기 설계 작성 |
