# Project AGENTS.md

## Project Goal

Build a writing-first, terminal-free Emacs package for pi.

Primary use case:
- stay in the current paper-writing buffer
- select a region or use the current paragraph
- ask pi to revise/rewrite text
- review the proposed change in Emacs
- accept or reject without leaving the main writing workflow

This project is **not** primarily a chat UI and **not** a terminal wrapper.

## Product Direction

Treat pi as an async **text rewrite engine** for the current buffer.

Primary workflow:
1. user invokes a command from the current buffer
2. package captures region/paragraph text
3. package sends a rewrite request to pi in the background
4. package shows original vs rewritten text in a review buffer with diff
5. user accepts or rejects

The current editing buffer is the center of the UX.

## Strong Requirements

- No embedded terminal UI
- No mandatory dedicated chat window for normal use
- Async operation
- Review before apply
- Conservative/safe apply semantics
- Minimal disruption to paper-writing flow

## Explicit Non-Goals for v1

- Full chat-first workspace
- Project-wide coding assistant features
- Tool-calling/file-editing workflows
- Persistent session trees/history
- Large feature surface
- Complex multi-buffer session UI

## Architectural References

### dnouri/pi-coding-agent
Use as reference for:
- terminal-free architecture
- launching `pi --mode rpc`
- JSON RPC process management
- rendering structured results in Emacs

Do **not** copy its chat/input two-window model as the primary UX.

### stevemolitor/claude-code.el
Use as reference for:
- current-buffer-oriented commands
- minibuffer/transient interaction style
- low-friction workflow from normal editing buffers
- confirm/apply patterns

Do **not** copy its terminal backend.

## Core Architecture

Keep the package small and structured around rewrite jobs.

### Main modules
- `pi.el` — public entry point, commands, customization
- `pi-session.el` — hidden pi process/session management
- `pi-rpc.el` — JSON RPC transport layer
- `pi-request.el` — rewrite job lifecycle and orchestration
- `pi-review.el` — review UI and accept/reject/apply

## Core Internal Abstractions

### Session
A hidden pi subprocess associated with a project root or file directory.

Suggested session key:
- project root if available
- otherwise current file directory

Why:
- preserve useful local context
- avoid a single global process for unrelated projects
- keep the process invisible to the user

### Rewrite Job
This is the main unit of work.

Each job should track at least:
- id
- session
- source buffer
- begin marker
- end marker
- original text
- instruction
- status
- result text
- error message
- review buffer

Use markers, not raw positions.

## Public Commands for v1

Keep command surface minimal.

- `pi-rewrite-region`
  - requires active region
  - prompts for instruction in minibuffer
- `pi-rewrite-dwim`
  - uses region if active
  - otherwise uses current paragraph
  - prompts for instruction
- `pi-review-accept`
- `pi-review-reject`

## Prompt Strategy

For v1, every request should be a narrow text rewrite task.

The package should instruct pi to:
- rewrite only the provided text
- follow the user instruction
- preserve meaning unless instructed otherwise
- return only the revised text
- avoid commentary and markdown fences unless explicitly requested

Avoid open-ended agent tasks for v1.

## Review UI

The source writing buffer remains primary.

Review should open in a temporary side buffer, not take over the whole workflow.

Suggested review contents:
- source buffer name
- user instruction
- original text
- revised text
- diff

Introduce a dedicated `pi-review-mode` for this buffer.

Suggested keys:
- `a` accept
- `r` reject
- `q` quit

## Apply Semantics

Applying a rewrite must be conservative.

Only auto-apply if:
- source buffer still exists
- markers are valid
- text between markers still matches the original captured text

If the source changed:
- do not silently apply
- preserve the proposed rewrite
- require explicit user action or rerun

Do not implement automatic merge logic in v1.

## Buffer Model

Visible buffers for v1:
- the source writing buffer
- a temporary review buffer

Hidden/internal buffers:
- pi process / RPC log buffer if needed

Avoid introducing chat buffers or input composition buffers in v1 unless clearly necessary.

## State Philosophy

Keep v1 nearly stateless.

Maintain only:
- active hidden sessions
- in-flight rewrite jobs
- temporary review state

Do not add persistence/session history unless explicitly needed.

## Implementation Style

- Prefer a clean, small architecture over feature breadth
- Keep concerns separated by module
- Optimize for correctness and low friction
- Build the writing/review loop first; everything else is secondary

## Design Principle

This project should feel like:
- “ask pi to revise this text here”

not like:
- “open a coding-agent terminal/chat app inside Emacs”
