# MASC Multi-Room Design

## Problem Statement

현재 MASC는 단일 방(Single Room) 모델:
- `.masc/` 폴더 = 1개의 방
- 여러 프로젝트/팀을 동시에 관리할 수 없음
- 방 목록에서 선택해서 입장하는 UX가 없음

## Goals

1. **방 목록 조회**: `masc_rooms_list` - 사용 가능한 방 목록
2. **방 생성**: `masc_room_create` - 새 방 만들기
3. **방 입장**: `masc_room_enter` - 특정 방으로 전환
4. **방 상태 유지**: 에이전트는 한 번에 하나의 방에만 존재

## Architecture

### Directory Structure (FileSystem Backend)

```
.masc/
├── rooms.json           # 방 레지스트리
├── current_room         # 현재 활성 방 이름
├── rooms/
│   ├── default/         # 기본 방 (마이그레이션 호환)
│   │   ├── agents/
│   │   ├── tasks/
│   │   ├── messages.json
│   │   └── locks/
│   ├── my-project-dev/    # 커스텀 방
│   │   ├── agents/
│   │   ├── tasks/
│   │   └── ...
│   └── personal/
└── config.json          # 전역 설정
```

### Redis Key Structure (Redis Backend)

```
MASC:{cluster}:rooms                     # Set of room names
MASC:{cluster}:room:{room_id}:agents     # Hash of agents
MASC:{cluster}:room:{room_id}:tasks      # Hash of tasks
MASC:{cluster}:room:{room_id}:messages   # List of messages
MASC:{cluster}:room:{room_id}:locks      # Hash of locks
```

### Type Definitions

```ocaml
(* 방 메타데이터 *)
type room_info = {
  id: string;               (* 고유 ID: slugified name *)
  name: string;             (* 표시 이름 *)
  description: string option;
  created_at: string;
  created_by: string option;  (* 생성한 에이전트 *)
  agent_count: int;         (* 현재 참여 에이전트 수 *)
  task_count: int;          (* 활성 태스크 수 *)
} [@@deriving yojson, show]

(* 방 레지스트리 *)
type room_registry = {
  rooms: room_info list;
  default_room: string;     (* 기본 방 ID *)
} [@@deriving yojson]

(* 확장된 config *)
type config = {
  backend_type: backend_type;
  base_path: string;
  redis_url: string option;
  node_id: string;
  cluster_name: string;
  current_room: string;     (* NEW: 현재 방 ID *)
}
```

### New MCP Tools

#### 1. `masc_rooms_list`

방 목록 조회.

**Parameters**: None

**Returns**:
```json
{
  "rooms": [
    {
      "id": "default",
      "name": "Default Room",
      "agent_count": 2,
      "task_count": 5,
      "created_at": "2025-01-10T12:00:00Z"
    },
    {
      "id": "my-project-dev",
      "name": "My Project Development",
      "agent_count": 0,
      "task_count": 3
    }
  ],
  "current_room": "default"
}
```

#### 2. `masc_room_create`

새 방 생성.

**Parameters**:
- `name` (required): 방 이름 (표시용)
- `description` (optional): 설명

**Returns**:
```json
{
  "id": "my-project-dev",
  "name": "My Project Development",
  "message": "✅ Room 'my-project-dev' created"
}
```

#### 3. `masc_room_enter`

방 입장 (전환).

**Parameters**:
- `room_id` (required): 방 ID

**Behavior**:
1. 현재 방에서 에이전트 leave 처리 (heartbeat 중지)
2. 새 방으로 전환
3. 새 방에 join (새 닉네임 부여)

**Returns**:
```json
{
  "previous_room": "default",
  "current_room": "my-project-dev",
  "nickname": "claude-swift-fox",
  "message": "✅ Entered room 'my-project-dev' as claude-swift-fox"
}
```

#### 4. `masc_room_delete`

방 삭제 (비어있을 때만).

**Parameters**:
- `room_id` (required): 방 ID
- `force` (optional): 에이전트 있어도 강제 삭제

## Migration Strategy

### Phase 1: Backward Compatible

기존 `.masc/` 구조를 `rooms/default/`로 취급:

```ocaml
let room_path config room_id =
  if room_id = "default" then
    (* Legacy: 기존 .masc/ 루트를 그대로 사용 *)
    config.base_path
  else
    Filename.concat config.base_path (Printf.sprintf "rooms/%s" room_id)
```

### Phase 2: Full Migration

신규 설치시 `rooms/` 구조로 생성:

```
.masc/
├── rooms/
│   └── default/
│       ├── agents/
│       └── ...
└── rooms.json
```

## Implementation Plan

### Files to Modify

| File | Changes |
|------|---------|
| `lib/types.ml` | `room_info`, `room_registry` 타입 추가 |
| `lib/backend.ml` | `config.current_room` 필드 추가 |
| `lib/room.ml` | room-aware 경로 함수들, 새 Room 관리 함수들 |
| `lib/tools.ml` | 새 MCP tool definitions |
| `lib/mcp_server.ml` | tool handlers |

### New Files

| File | Purpose |
|------|---------|
| `lib/room_manager.ml` | Room CRUD operations |

## UX Flow

```
┌─────────────────────────────────────────────────────┐
│  masc_rooms_list                                    │
│  ─────────────────────────────────────────────────  │
│  📁 Rooms (3)                                       │
│                                                     │
│  ● default        [2 agents, 5 tasks] ← current    │
│  ○ my-project-dev   [0 agents, 3 tasks]              │
│  ○ personal       [1 agent,  0 tasks]              │
│                                                     │
│  > masc_room_enter my-project-dev                    │
└─────────────────────────────────────────────────────┘
```

## Questions

1. **방 간 에이전트 공유?**
   - Option A: 에이전트는 한 번에 하나의 방에만 존재 (권장)
   - Option B: 에이전트가 여러 방에 동시 참여 가능

2. **방 권한?**
   - Option A: 모든 방 공개 (현재 설계)
   - Option B: 방별 접근 권한 (나중에 추가)

3. **기본 방 자동 생성?**
   - `masc_init` 시 `default` 방 자동 생성

## Timeline

- [ ] Phase 1: `room_info` 타입 + `rooms_list` (읽기 전용)
- [ ] Phase 2: `room_create`, `room_enter`
- [ ] Phase 3: 방별 격리 검증 + 테스트
- [ ] Phase 4: Redis backend 지원
