# Claude API diagnostic frames and retry safety

## Observed symptom

The six-hour runtime audit ending at 2026-09-07T02:58:23Z found quota
failures with `tool_effect_attempted=false response_emitted=true`.
One directly inspected record is
`<base-path>/.masc/keepers/rondo/raw-traces/turn-1788749762345-2b29-000089.jsonl:6`:

```text
Claude Code subscription quota blocked (rate_limit=rejected api_status=429 tool_effect_attempted=false response_emitted=true) (retry_after: 6193.428s)
```

This normalized trace proves the emitted-response classification, but does
not retain the original assistant envelope. It cannot prove that this
particular response was an API diagnostic.

## Producer and consumer evidence

The installed Claude Code 2.1.263 executable was inspected directly. Its
SHA-256 is `ef5d2909c8af49f31ab6d5487e90316777bc2fac170adfe8160716caa8aaf4f9`.
The assistant envelope schema declares `is_api_error_message` as an optional
boolean describing a wrapped API error. Its `Yct` producer contains:

```javascript
is_api_error_message: e.isApiErrorMessage === !0 ? !0 : void 0
```

Before this change, `Runtime_claude_code.parse_assistant` ignored that field,
and `await_terminal` emitted every text block as `Text_delta`, set
`response_emitted`, and replaced the measured model. The keeper's typed
retry path requires both effect and response observations to be false.
Thus a producer-marked diagnostic alone could fence otherwise safe quota
failover or context-overflow recovery. This is established from the producer
and consumer contracts; attribution of the specific live trace remains an
inference.

## Change and verification boundary

The optional wire boolean now parses into private `Model_response` or
`Api_error_diagnostic` variants. Missing and false mean ordinary assistant;
all non-boolean values fail parsing. Diagnostics do not start a response
stream, emit or accumulate text, contribute synthetic usage, or replace a
measured model. Previous response observations and native/MCP effects,
including tool blocks sharing a diagnostic frame, remain intact. The
terminal result and rate-limit event still classify errors and reset times.

Ten registered CLI fixture cases cover quota/reset preservation, prior text,
prior and mixed native/MCP effects, overflow recovery, terminal error detail,
ordinary identical prose with missing/false flags, malformed flag values,
valid text after a diagnostic, and preservation of measured model/text.

Static verification: `git diff --check`, `bash scripts/check-variants.sh`,
and an enabled `ocamlformat` parse of both changed OCaml files passed.
These are not type checking or test execution. Local builds and fixture
execution were not run under the constitution execution protocol. CI is
scheduled by the coordinating agent; deployment and live recovery remain
unverified. This change has no dashboard component or browser measurement.

Independent review: `review_claude_error_frames` established the producer
contract; `respond_claude_review` implemented the response. The root agent
independently read the production diff and ten CLI cases and found no
blocking issue. These inherited-model reviews are not cross-model review.
