# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## NEVER manipulate third-party internals from a high-level layer

**ABSOLUTE RULE: a high-level layer must never touch a third-party dependency's internals directly. Every third-party API call must go through a dedicated wrapper module that owns the integration boundary.** "Third-party" here means anything outside the module itself — external packages, language-tool integrations, shell programs, etc.

The shape is always the same, regardless of which dependency is in play:

- One file owns the integration boundary for a given dependency.
- Every other file expresses its intent in terms of the module's own semantics and routes through the owning file's wrapper API.
- The dependency-specific bookkeeping (data-structure shape, naming conventions, undocumented-behavior workarounds, version skew) lives inside the owning file, never sprinkled across the high-level layer.

If a high-level call site needs a behavior the owning file does not yet expose, STOP and surface to the user with:

1. The exact third-party API the code wants to call.
2. The call site (file:line) that would call it.
3. A proposed wrapper name + signature in the owning file, or the owning file itself if no owner exists yet.
4. A note on whether any existing wrapper looks close enough to extend.

Wait for the user to decide whether to extend an existing wrapper, add a new one, or take a different approach. Do NOT silently sprinkle the third-party call at the high-level site, even "just this once".

Why this is absolute:

- Direct third-party calls at high-level sites are abstraction leaks. High-level code is supposed to express the module's own semantics, not the dependency's internal bookkeeping.

- Leaks create load-bearing dependencies on a third party's undocumented or version-specific behavior. The fix for any one such quirk then has to be applied at N call sites instead of one wrapper.

- They also pin the module to the current dependency choice. A future swap (different backend, different tool) touches N files instead of one.

- They defeat the testing model. The wrappers ARE the mock boundary — without them, every call site has to be mocked independently, and tests of high-level logic end up encoding dependency-specific behavior that drifts when the dependency upgrades.

## No Silent Fallbacks — Fail Hard on Invariant Violations

**ABSOLUTE RULE: Do not introduce ANY "fallback" behavior.** Under no circumstances — without **explicit, per-case permission from the user**, and only when the fallback is *absolutely* necessary — may code fall back to an alternative value, default, or code path when the primary input/lookup/precondition is missing or fails. **Always** prefer a loud error and a hard failure. Assume the answer is "no fallback" and propose the failure mode to the user; wait for explicit approval before writing any fallback. Do not suggest a fallback unless asked, and do not smuggle one in under names like "default", "graceful degradation", "sensible behavior when …", or "keep existing callers working".

**Never silently fall back, skip, or no-op when a precondition fails.** If an invariant is violated, **immediately fail loudly** with a logged entry AND user-visible feedback (`user-error`, `error`, or at minimum a `message` that reaches the echo area). **Never fall back to an alternative code path** — the operation must abort entirely.

**Do not commit state changes before the failure point.** If an operation involves multiple steps, validate all preconditions before mutating any state. If validation fails partway through, the system state must remain unchanged on failure. Always commit concrete changes when finished. Commit frequently.

Anti-patterns to reject:
- `(when-let ((x (lookup))) BODY)` where BODY is user-expected behavior. If `lookup` returns nil, the caller needs to know.
- State mutations that run regardless of whether the upstream operation succeeded.
- `(ignore-errors ...)` without a companion log of what was swallowed.
- `or`-chained defaults that mask missing data: `(or (get-thing :key) (default-thing))`.
- Early returns that hide a failed precondition instead of signaling it.
- Returning nil from a resolution function and letting the caller silently degrade to a default.
- `if`/`cond` branches that pick an ambient value when an explicit input is absent. If X is required, demand X — do not synthesize it.
- Comments or commit messages that contain the phrases "fall back", "falls back", "fallback", "default to", or "for backwards compatibility" describing runtime behavior. Treat these as review blockers.

The only acceptable silent no-op is one whose contract **explicitly requests** it: a best-effort cleanup where failure is known to be recoverable, or a `lookup-or-nil`-style query function. In those cases, document the contract in the docstring so callers know what they're getting. Even then, prefer an explicit error over a silent no-op unless the user has signed off on the recoverable-failure semantics.

When in doubt: fail loudly. When a precondition fails: abort entirely. When tempted to write a fallback: stop, surface the situation to the user, and wait for explicit authorization.

## Testing

**Zero tolerance for test failures.** Every test failure is a real bug that must be fixed before your work is done. **There is NO such thing as a "pre-existing" failure — not under ANY circumstances, EVER.** Do not investigate whether a failure predates your work. Do not check git history. Do not stash, checkout, or touch git state to "verify" it was already broken. Do not rationalize, dismiss, categorize, defer, or explain away any test failure for any reason whatsoever. If a test fails, fix it. Every failing test is your responsibility the moment you observe it. Never report work as complete while any test is failing.

## No External Processes or External State in Tests

**ABSOLUTE RULE: Tests must NEVER invoke an external process and must NEVER mutate any external state.** This includes — but is not limited to — `git`, `gh`, `curl`, any user-installed binary, the system clipboard, the desktop notification system, environment variables outside the test's own dynamic let-binding, and any filesystem path outside `temporary-file-directory`. Tests are pure. No subprocess. No `call-process`, `start-process`, `process-file`, `shell-command`, `shell-command-to-string`, `make-process`, `async-shell-command`. No `write-region` or `make-directory` outside of `temporary-file-directory`, and even those should be a last resort — prefer in-memory state.

**Why:**

- Tests that shell out are slow, flaky, and platform-dependent.

- Tests that mutate external state (e.g., creating real git branches, writing real files) pollute the developer's machine and other tests.
  - A real recovered incident: a test suite that ran `git -C $TEMP init` and `git -C $TEMP commit` somehow ended up writing branches into the developer's actual repo's `.git/refs/heads/`, leaving the worktree in a half-cherry-picked state.
  - The root cause is fundamentally that the tests were invoking real `git` at all — once the boundary is crossed, the blast radius is impossible to bound by inspection.

- Tests that depend on installed binaries fail differently on CI, in containers, on a coworker's machine, and after a tool upgrade.

- Mocked tests run in milliseconds, are deterministic, and document the exact contract between production code and the external boundary.

**Required pattern:**

1. Every external-process or external-state call in production code is wrapped by a dedicated single-purpose function.
   - The wrapper does ONE thing: invoke the external call.
   - The wrapper does NOT contain conditional logic, parsing, retries, formatting, or any other business logic — that belongs in callers that the test exercises directly without mocking.

2. Tests stub the wrapper via `cl-letf` (or equivalent) and supply fixture return values.
   - Tests assert against the production lisp behavior, NOT the external system's behavior.

3. **No external call may exist in production code outside such a wrapper.** If you find a bare `(shell-command-to-string ...)` or `(call-process ...)` in production code, extract it into a wrapper first; only then write the calling logic.

4. **No external call may exist in test code at all.** Not even via a "test-only helper" macro.

**Prohibited anti-patterns:**

- `(call-process ...)` or `(shell-command-to-string ...)` inside a test file or test helper.
- "Temp repo" macros that build up state via real external invocations — even when scoped via a temp directory. The blast radius is bigger than it looks.
- Tests that depend on the test runner's CWD being inside a git repo.
- Tests that mutate `~/` paths or any path under `$HOME` other than `temporary-file-directory`.

**When you encounter a test that violates this rule:** stop, refactor the production code to introduce the wrapper if it doesn't already exist, then rewrite the test to mock the wrapper. Do not add new tests that perpetuate the pattern.

### We test code behavior, not external code

The corollary to "no external processes in tests" is: **tests exist to cover elisp behavior, not external program behavior.** If a candidate test would exclusively exercise a non-elisp artifact (a shell script, an installed binary's command-line surface, a remote service, etc.), it does NOT belong in the ERT suite.

Concretely: do not add a test whose body is "spawn the external thing, then assert on what it did." Mocking the external call in that situation reduces the test to asserting nothing of value (the entirety of the contract under test lives outside lisp). The right path is:

- If lisp dispatches to the external thing, test the lisp dispatch — mock the wrapper and assert the dispatch logic.

- If you genuinely want to cover the external thing's behavior, write that coverage as a separate runner (a `make test-install-bash` target, a CI job, a hand-run harness). It does not belong in the ERT batch invoked by the pre-commit hook.

## Paren Checking

To verify parenthesis balance in an `.el` file (skipping strings and comments):

```bash
python3 .claude/check-parens.py <file.el>
```

## No Redundant Mechanisms

Never maintain two mechanisms for the same thing. Redundancy adds complexity, obscures which path is authoritative, and creates subtle divergence bugs. If a new approach replaces an old one, **delete the old one** — do not keep it "as a fallback." If the new approach isn't trusted enough to stand alone, it's not ready to ship.

Example: Claude Code hooks (`session_start`, `stop`, `prompt_submit`) are the sole source of session IDs and lifecycle events. Do not also scan session files, watch terminal titles for readiness, or poll for state that hooks already deliver. One mechanism, one source of truth.

## No Duplicated or Mirrored Code — Always Extract Shared Helpers

**ABSOLUTE RULE: Never duplicate, mirror, or copy-paste code when extraction into a shared helper is possible. Always extract.** This applies to function bodies, prompt/template strings, defconst content, conditional branches, repeated `let*` blocks, parallel test-setup boilerplate — anything. If two call sites share more than trivial structure, the shared structure belongs in a helper, and the call sites become thin dispatches that vary only in their parameters.

The bar is intentionally low: if a future reader would look at two functions and think "these look almost identical," they should be one function with parameters. "Almost identical" is the smell — do not let it ship.

Why this is absolute:
- Mirrored code drifts. Every "almost identical" pair becomes a "subtly divergent" pair within a few edits — one site picks up a fix or a new arg, the other doesn't, and the bug is invisible until it bites.
- Mirrored code multiplies the test surface. Two parallel functions need two parallel test suites; one helper needs one test suite plus thin per-caller tests.
- Mirrored code obscures intent. A reader cannot tell whether the duplication is intentional (different requirements) or accidental (lazy copy-paste). Extraction forces that decision to be explicit at the parameter list.
- Mirrored code creates fertile ground for the "fix one, forget the other" class of bug — which is doubly bad in conjunction with the No-Silent-Fallbacks rule, because both sites continue silently doing the wrong thing.

Required process when adding a new variant of an existing pattern:

1. **Before writing the new variant, identify the existing one.** Read it. Look for what would differ vs. what would stay the same.
2. **If anything stays the same, extract first.** Pull the shared body into a private helper. Make the differing parts parameters. Then rewrite the existing call site through the helper *as a separate refactor commit*, run the tests to prove the refactor is behavior-preserving, and only then add the new variant on top.
3. **Test the helper directly,** in addition to the wrappers. The wrappers are thin and tested via end-to-end behavior; the helper carries the contract and deserves its own focused unit tests for the contract (validation, interpolation, edge cases).
4. **The wrappers must be trivial after extraction.** Each wrapper should be ~3–8 lines: docstring + `(interactive)` + a single call into the helper with literal arguments. If the wrapper is doing anything else, push that into the helper too.

Anti-patterns to reject:
- Two `defun`s whose bodies are 80%+ the same and differ only in 2–3 literal values (path, label, string).
- Two `defconst`s whose strings share more than a sentence of template structure and differ only in interpolated tokens — extract a builder function and call it from both `defconst`s.
- "I'll just copy this and tweak it" as a working assumption when implementing a variant — that *is* the moment to extract.
- A new variant added without first refactoring the original through a shared helper.
- Comments like "mirrors X" / "parallel to X" / "based on X" used as a substitute for actually sharing code with X — those comments are a confession that the code should have been extracted.
- Test files where two `ert-deftest`s differ only in which function is called and which expected literal is asserted — extract a helper, parameterize, or write a single table-driven test.

The ONLY acceptable reason to leave near-duplication in place is that the user has been told what the shared helper would look like and has *explicitly* opted to keep the duplication for a stated reason. Default to extraction; ask if unsure.

## Comment Non-Obvious Code

**ALWAYS comment any change whose reasoning isn't immediately obvious from the code itself — even if it's only slightly non-obvious.** The bar is low on purpose: if a future reader (or a future you) would have to re-derive *why* the line is shaped the way it is, leave a comment that says why. Examples that always warrant a `WHY:` comment:

- Load-bearing side effects of a call (e.g. a function called primarily for one purpose whose secondary effect is depended on elsewhere — name the dependent site).
- Reliance on an external package's undocumented or implicit behavior.
- Ordering constraints between statements that are not enforced by data flow.
- A choice between two plausible approaches where the rejected alternative has a subtle failure mode.
- A guard, fallback, or `ignore-errors` whose absence would cause a specific concrete bug — name the bug.

The comment must explain *why*, not *what*. "Calls foo before bar" describes the code; "foo must run first because bar reads state foo writes via hook X" is the comment. If you can delete the comment without losing information a reader needs, it shouldn't have been written; if a reader would have to git-blame or grep to understand the line, the comment is required.

## Git

When asked to make changes, commit your work when done. Commit freely and often. **Never** rebase, pull, merge, push, or run any other mutating git commands without explicit instruction from the user.

## AGENTS.md Updates

Keep entries minimal — one short sentence or a brief code block per rule.
