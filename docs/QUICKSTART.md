# MASC Quick Start Guide

## 시작

```bash
# 1. PostgreSQL 모드로 서버 시작
export MASC_STORAGE_TYPE=postgres
export MASC_POSTGRES_URL=$POSTGRES_URL
cd ~/me/features/masc-mcp && ./start-masc-mcp.sh --http

# 2. 상태 확인
curl http://127.0.0.1:8935/health
```

---

## 지원 프로토콜

| 프로토콜 | 포트 | 바이너리 | 용도 |
|----------|------|----------|------|
| **HTTP/SSE** | 8935 | main.exe / main_eio.exe | MCP 클라이언트 (Claude Code) |
| **gRPC** | 9936 | masc_grpc_eio.exe | gRPC 기반 Agent 통신 |
| **WebRTC** | - | (실험) | P2P 실시간 협업 |

---

## 시나리오별 설정

### 시나리오 1: 로컬 싱글 머신 (Claude + Gemini)

```bash
# 같은 맥에서 Claude Code와 터미널 Gemini 협업
export MASC_STORAGE_TYPE=fs  # 로컬 파일시스템
./start-masc-mcp.sh --http --port 8935
```

**MCP 설정** (`.mcp.json`):
```json
{
  "mcpServers": {
    "masc": {
      "url": "http://127.0.0.1:8935/mcp"
    }
  }
}
```

### 시나리오 2: 멀티머신 협업 (MacBook + Mac Studio)

```bash
# 두 머신 모두 같은 PostgreSQL 사용
export MASC_STORAGE_TYPE=postgres
export MASC_POSTGRES_URL="postgresql://user:pass@railway-host:5432/db"
export MASC_CLUSTER_NAME=me  # 클러스터 이름 통일!

./start-masc-mcp.sh --http
```

### 시나리오 3: gRPC 통신

```bash
# HTTP + gRPC 동시 실행
./start-masc-mcp.sh --http --port 8935 &
dune exec ./bin/masc_grpc_eio.exe -- --port 9936
```

**gRPC 클라이언트 예시** (Python):
```python
import grpc
from masc_pb2_grpc import MASCServiceStub
from masc_pb2 import StatusRequest

channel = grpc.insecure_channel('localhost:9936')
stub = MASCServiceStub(channel)
response = stub.GetStatus(StatusRequest(room="me"))
print(response.agents)
```

---

## 실전 워크플로우

### 워크플로우 1: Task 기반 협업

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Claude    │     │   Gemini    │     │   Codex     │
│  (MacBook)  │     │ (Terminal)  │     │  (Server)   │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       │  masc_claim       │                   │
       │ ───────────────►  │                   │
       │  "task-123"       │                   │
       │                   │                   │
       │                   │  masc_status      │
       │                   │ ◄─────────────────│
       │                   │                   │
       │  masc_done        │                   │
       │ ───────────────►  │                   │
       │                   │                   │
       │                   │  masc_claim       │
       │                   │ ◄─────────────────│
       │                   │  "task-124"       │
```

**MCP Tool 호출**:
```typescript
// 1. Task 시작
await masc_claim({ task_id: "implement-feature-x" })

// 2. 작업 완료
await masc_done({ task_id: "implement-feature-x" })

// 3. 브로드캐스트
await masc_broadcast({ message: "@gemini 다음 차례야" })
```

### 워크플로우 2: Git Worktree 격리

```bash
# Claude: worktree 생성
masc_worktree_create --branch feat/auth --agent claude

# 작업 진행 (충돌 없음)
# ...

# PR 생성 후 정리
gh pr create --draft
masc_worktree_remove --branch feat/auth
```

### 워크플로우 3: Portal A2A 직접 통신

```typescript
// Claude → Gemini 직접 요청
await masc_portal_open({ target: "gemini" })
await masc_portal_send({
  target: "gemini",
  request: "이 코드 리뷰해줘",
  context: { file: "src/auth.ts" }
})

// Gemini 응답 대기
const response = await masc_portal_status({ target: "gemini" })
```

---

## 트러블슈팅

### Redis Blocking 문제
```
증상: MCP 요청이 느려지거나 타임아웃
원인: Lwt 서버의 Redis 동기 호출
해결: PostgreSQL 모드로 전환

export MASC_STORAGE_TYPE=postgres
./start-masc-mcp.sh  # Eio 기본 런타임 (Lwt는 deprecated)
```

### 연결 거부
```
증상: Connection refused on port 8935
확인: curl http://127.0.0.1:8935/health
해결: 서버가 실행 중인지 확인, 포트 충돌 체크
```

### 클러스터 불일치
```
증상: 다른 머신 에이전트가 안 보임
확인: MASC_CLUSTER_NAME이 동일한지 확인
해결: 모든 머신에서 같은 클러스터 이름 사용
```

---

## 모니터링

### 실시간 상태 확인
```bash
# Health check
curl -s http://127.0.0.1:8935/health | jq

# Room 상태
curl -s -X POST http://127.0.0.1:8935/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"masc_status"}}'
```

### 비용 추적
```bash
# 세션별 토큰 사용량
dune exec ./bin/masc_cost.exe
```

---

## 관련 문서

- [MASC-V2-DESIGN.md](./MASC-V2-DESIGN.md) - 아키텍처 상세
- [README.md](../README.md) - 설치 및 빌드
- [HOLONIC-ARCHITECTURE.md](./HOLONIC-ARCHITECTURE.md) - 에이전트 계층 구조
