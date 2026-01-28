# Dashboard Integration Spec (MASC MCP)

## 목표
- 외부 대시보드에서 **상태/작업/에이전트/메시지**를 안전하게 읽는다.
- SSE 스트림으로 변경 이벤트를 받는다.
- 인증/권한을 강제한다.

## 기본 정보
- Base URL: `http://127.0.0.1:8935`
- REST Base: `/api/v1`
- SSE: `/sse`
- MCP: `/mcp`

## 인증
- HTTP Header
  - `Authorization: Bearer <token>`
  - `X-MASC-Agent: <agent_name>`
- SSE (EventSource는 header 불가)
  - `/sse?agent=<agent_name>&token=<token>`
- Auth 비활성 시에도 위 헤더는 허용한다.

## REST API

### GET /api/v1/status
- 응답
  - `cluster`, `project`, `tempo_interval_s`, `paused`

### GET /api/v1/tasks
- Query
  - `status`: `todo|claimed|in_progress|done|cancelled` (optional)
  - `limit`: default 50
  - `offset`: default 0
- 응답
  - `tasks`: list
  - `limit`, `offset`, `total`

### GET /api/v1/agents
- Query
  - `status`: `active|busy|inactive` (optional)
  - `limit`: default 50
  - `offset`: default 0
- 응답
  - `agents`: list
  - `limit`, `offset`, `total`

### GET /api/v1/messages
- Query
  - `since_seq`: default 0
  - `limit`: default 20
  - `agent`: filter by sender (optional)
- 응답
  - `messages`: list
  - `limit`, `since_seq`, `total`

### GET /api/v1/credits
- 응답
  - credits dashboard JSON

## SSE (Server-Sent Events)
- Endpoint: `/sse`
- 인증: `?agent=...&token=...` (권장)
- 이벤트 형식
  - `event: message`
  - `data: <JSON>`
- 주요 타입
  - `masc/broadcast`
  - `masc/task_update`
  - `masc/agent_joined`
  - `masc/agent_left`
  - `masc/tempo_change`

## 오류 규약
- REST 응답은 JSON으로 반환
- 인증 실패
  - 401 Unauthorized
  - 메시지: `{"error":"unauthorized"}`
- 권한 부족
  - 403 Forbidden
  - 메시지: `{"error":"forbidden"}`

## 통합 체크리스트
- Authorization 헤더 또는 SSE query 적용
- REST 페이지네이션 적용
- SSE 이벤트 타입 매핑
- 장애 대비 폴링 fallback (5s)
