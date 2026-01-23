# MASC Interrupt Design (LangGraph 패턴)

> Human-in-the-loop 워크플로우를 위한 Interrupt/Resume 시스템

**Status**: Design Phase
**Created**: 2026-01-04
**Author**: BALTHASAR (Claude)

## 개요

LangGraph의 `interrupt()` 패턴을 MASC에 적용하여, 위험한 작업 전 사용자 승인을 받는 기능.

**참고**: [LangGraph Human-in-the-Loop](https://langchain-ai.github.io/langgraphjs/concepts/human_in_the_loop/)

## 핵심 개념

### 1. Interrupt 상태

체크포인트에 `status` 필드 추가:

```
pending → in_progress → [interrupted] → completed
                              ↓
                        [rejected] → (워크플로우 종료)
```

### 2. 사용 패턴

```bash
# 1. Approve/Reject 패턴 - 위험 작업 전 승인
masc-checkpoint --task-id task-030 --step 2 \
  --action "데이터베이스 삭제" \
  --interrupt "정말 삭제하시겠습니까?" \
  --agent claude

# 2. 사용자가 승인
masc-checkpoint --task-id task-030 --approve

# 3. 또는 거절
masc-checkpoint --task-id task-030 --reject --reason "취소됨"
```

**저장 백엔드 (auto)**:
- 기본값은 **auto**: `NEO4J_URI`가 설정되어 있으면 Neo4j에 저장, 아니면 `.masc/state/checkpoints/`에 파일로 저장
- 강제 전환: `MASC_CHECKPOINT_BACKEND=neo4j|filesystem`
- 작업 디렉토리 지정: `--masc-dir .masc` (MCP 서버는 룸 경로에 맞춰 자동으로 주입)

### 3. MCP 도구

| Tool | 설명 |
|------|------|
| `masc_interrupt` | 워크플로우 일시정지 + 승인 요청 메시지 |
| `masc_approve` | 중단된 워크플로우 승인/재개 |
| `masc_reject` | 워크플로우 거절/종료 |

## Neo4j 스키마 확장

```cypher
// Interrupted Checkpoint
(:Checkpoint {
  id: "cp-task-030-2-...",
  status: "interrupted",         // NEW: pending | in_progress | interrupted | completed | rejected
  interrupt_message: "정말 삭제하시겠습니까?",
  interrupt_at: datetime(),
  resume_value: null,            // 재개 시 사용자 입력값
  resumed_at: null
})

// Interrupt 이벤트 노드
(:InterruptEvent {
  checkpoint_id: "cp-task-030-2-...",
  message: "정말 삭제하시겠습니까?",
  created_at: datetime(),
  resolved_at: null,
  resolution: null,              // "approved" | "rejected"
  resolved_by: null,             // 승인한 사용자/에이전트
  reason: null                   // 거절 사유
})
```

## 워크플로우 예시

### 시나리오: 데이터 삭제 승인

```
에이전트 → [Step 1: 삭제 대상 확인] → 체크포인트
        → [Step 2: INTERRUPT - "10,000건 삭제 예정"] → 대기
                    ↓
        사용자: /approve task-030
                    ↓
        → [Step 3: 삭제 실행] → 체크포인트
        → [Step 4: 완료 보고] → 체크포인트
```

### 시나리오: 외부 API 호출 승인

```
에이전트 → [결제 API 호출 준비]
        → INTERRUPT: "₩50,000 결제 진행?"
        → 사용자 승인 대기
        → 승인 시 API 호출
        → 거절 시 워크플로우 종료
```

## 구현 우선순위

### Phase 1: 기본 Interrupt (MVP)
- [ ] Checkpoint에 `status` 필드 추가
- [ ] `masc_interrupt` MCP 도구
- [ ] `masc_approve` / `masc_reject` 도구
- [ ] Neo4j 스키마 마이그레이션

### Phase 2: 알림 통합
- [ ] Slack 알림 연동 (승인 요청)
- [ ] 대시보드에 Pending Interrupts 표시
- [ ] 이메일 알림 (선택)

### Phase 3: 고급 기능
- [ ] 타임아웃 자동 거절
- [ ] 다중 승인자 (2-of-3 승인)
- [ ] 승인 권한 레벨

## OCaml 구현 스케치

```ocaml
(* types.ml *)
type checkpoint_status =
  | Pending
  | InProgress
  | Interrupted of { message: string; requested_at: float }
  | Completed
  | Rejected of { reason: string; rejected_at: float }

(* interrupt tool handler *)
let handle_interrupt ~task_id ~step ~message ~agent =
  let checkpoint = create_checkpoint ~task_id ~step ~agent
    ~status:(Interrupted { message; requested_at = Unix.time () }) in
  save_to_neo4j checkpoint;
  broadcast_to_masc ~agent
    (Printf.sprintf "⏸️ Interrupt: %s (task: %s, step: %d)"
      message task_id step);
  Ok { waiting_for_approval = true; checkpoint_id = checkpoint.id }
```

## 관련 문서

- [CHECKPOINT-SCHEMA.md](./CHECKPOINT-SCHEMA.md) - 체크포인트 스키마
- [LangGraph Interrupts](https://docs.langchain.com/oss/python/langgraph/interrupts) - 원본 패턴

---

**다음 단계**: Phase 1 구현을 위한 태스크 생성
