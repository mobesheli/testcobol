# Dashboard vs. COBOL Requirements Cross-Check

**Scope**: Validate whether the FastAPI dashboard at `/dashboard` meets the functional behaviours defined by the legacy COBOL programs (`main.cob`, `operations.cob`, `data.cob`) and their BRD/FRD requirements.  
**Date**: 2024-02-15  
**Reviewed Components**:
- COBOL sources: `main.cob`, `operations.cob`, `data.cob`
- Requirements: `docs/Brd.md`, `docs/Frd.md`
- Modern stack: REST endpoints (`python-accounting-app/src/accounting/api/endpoints.py`) and dashboard UI (`python-accounting-app/src/accounting/api/frontend.py`)

---

## Summary Matrix

| Requirement | Source | Compliance | Notes |
| --- | --- | --- | --- |
| BR-01 Menu Navigation | `main.cob` / BRD | ⚠️ Partial | Dashboard replaces the menu with cards; all operations remain reachable, but the explicit “loop until exit” UX and exit confirmation message are absent. |
| BR-02 Input Validation | BRD | ⚠️ Partial | Amount inputs validate positivity and range (`min=0.01`, `max=999999.99`). Menu selection validation is not applicable in the web UX. |
| BR-03 Balance Inquiry | `operations.cob` | ✅ Compliant | `/dashboard` loads balance via `GET /api/v1/account/balance` and supports manual refresh. |
| BR-04 Credit Processing | `operations.cob` | ✅ Compliant | Credit form posts to `/credit`, shows success toast, refreshes balance/history. |
| BR-05 Debit Processing | `operations.cob` | ✅ Compliant | Debit form posts to `/debit`, handles insufficient funds responses without mutating balance. |
| BR-06 Data Persistence | `data.cob` / BRD | ⚠️ Partial | API persists immediately to SQLite/Postgres. UI assumes persistence succeeds; no failure fallback messaging beyond generic toasts. |
| BR-07 Precision Handling | BRD | ✅ Compliant | Inputs enforce 2-decimal steps and JS converts to integer cents before calling API. |
| BR-08 Exceptions | BRD | ⚠️ Partial | Invalid amounts trigger toasts. API validation errors (422) surface as `[object Object]` because FastAPI returns structured detail. |
| BR-09 Amount Boundaries | BRD | ✅ Compliant | UI enforces min/max; API rejects out-of-range values. |
| VR-01 Menu Selection | FRD | ⚠️ Partial | Not applicable to dashboard UX; menu replaced by buttons. |
| VR-02 Debit Sufficiency | FRD | ✅ Compliant | API logic preserves balance and returns insufficiency message that the UI displays. |
| VR-03 Amount Format | FRD | ✅ Compliant | Form constraints plus API validation ensure currency rules. |
| VR-04 Session Continuity | FRD | ⚠️ Partial | Browser session is stateless; dashboard stays available, but there is no explicit session loop or exit acknowledgement. |
| VR-05 Immediate Persistence | FRD | ✅ Compliant (backend) | Repository writes new balance before the UI refresh; behaviour mirrors COBOL semantics. |
| VR-06 Positive Amount | FRD | ✅ Compliant | Client-side guard plus API enforcement prevent zero/negative entries. |

Legend: ✅ Compliant · ⚠️ Partially Compliant · ❌ Non-Compliant · N/A Not Applicable

---

## Detailed Findings

### BR-01 / VR-01 – Menu Navigation & Selection
- **COBOL expectation**: Looping text menu (`main.cob:12-35`) continually prompts until the user chooses Exit (4). Invalid selections display an error and redisplay the menu.
- **Dashboard behaviour**: The landing page displays cards for balance inquiry, credit, and debit without a menu loop. There is no explicit exit mechanism or session flag.
- **Impact**: Functionality is accessible, but the user journey diverges from the original requirement that called for an interactive menu and exit messaging. Consider adding a visible “Sign out/Exit” affordance or documentation noting the change.

### BR-02 / VR-03 / VR-06 – Input Validation
- **COBOL expectation**: Amounts must be numeric, positive, and within `0.01–999,999.99`.
- **Dashboard behaviour**: Input fields enforce `step="0.01"`, `min="0.01"`, and `max="999999.99"` (`frontend.py:281-315`). `dollarsToCents` rounds to integer cents before submission.
- **Gaps**: No inline helper text for maximum values; invalid API responses return a generic toast. Enhancing messaging would improve parity with COBOL’s “enter a valid amount” prompts.

### BR-03 – Balance Inquiry
- **COBOL expectation**: Reads balance and displays without modifying storage.
- **Dashboard behaviour**: On load and on refresh buttons, calls `/api/v1/account/balance` and renders `balance_display`. No client-side mutations occur.
- **Verdict**: Behaviour matches the legacy requirement.

### BR-04 / BR-05 – Credit & Debit Processing
- **COBOL expectation**: Prompt for amount, read current balance, apply operation, persist, and display confirmation or insufficiency message.
- **Dashboard behaviour**: Forms capture USD values, convert to cents, and submit to REST endpoints. Success triggers balance/history refresh; insufficient funds present the API’s message.
- **Verdict**: Core behaviour aligns with COBOL rules. The toast presentation is modernized but still communicates required outcomes.

### BR-06 / VR-05 – Data Persistence
- **COBOL expectation**: Immediate write-back to `STORAGE-BALANCE` after successful transactions.
- **Dashboard behaviour**: API writes via SQLAlchemy repository before responses return; the UI assumes persistence succeeded and reflects new values after API confirmation.
- **Gap**: The dashboard lacks explicit error handling for database failures (e.g., network/storage errors). Consider surfacing persistence issues distinct from validation failures.

### BR-07 – Precision Handling
- **COBOL expectation**: Two decimal places, no floating-point drift.
- **Dashboard behaviour**: Amounts are converted to integer cents (`Math.round(parseFloat(value) * 100)`), preventing precision loss. Responses display formatted amounts from the API.

### BR-08 – Exception Messaging
- **COBOL expectation**: Specific console messages for invalid menu choices and insufficient funds.
- **Dashboard behaviour**: Custom toasts communicate invalid/empty amounts, success, and insufficiency. However, schema validation errors (422) emit `[object Object]` because the FastAPI error payload is not stringified.
- **Recommendation**: Normalize API error structures in the UI for clearer messaging, maintaining the original system’s clarity.

### BR-09 – Amount Boundaries
- **COBOL expectation**: Enforce both lower and upper bounds on transaction amounts.
- **Dashboard behaviour**: Client-side max attribute plus server-side validation meet the boundary requirement.

### VR-04 – Session Continuity
- **COBOL expectation**: Loop persists until exit command; user sees exit message.
- **Dashboard behaviour**: Persistent single-page app with no exit control. Users close tab/browser manually.
- **Recommendation**: Document the UX change or add an explicit “End Session” control to mimic COBOL behaviour.

---

## Additional Observations
- **Transaction History**: The dashboard introduces a modernization feature absent from COBOL, aligning with BRD pain point remediation.
- **Testing Consideration**: Automated tests currently fail without running migrations (`create_tables()`), which may affect assurance of persistence rules. Integrate DB setup into test workflows when validating compliance.
- **Accessibility**: Forms rely on color cues for status; consider adding ARIA attributes or text for improved accessibility.

---

## Recommended Follow-Up
1. **Improve error normalization** so FastAPI validation messages render user-friendly text instead of `[object Object]`.
2. **Clarify session lifecycle** via documentation or UI control mirroring COBOL’s exit messaging.
3. **Enhance inline guidance** on amount limits to mirror original console prompts.
4. **Add persistence error handling** (e.g., display a dedicated toast when network/database failures occur) to cover edge cases the COBOL console inherently surfaced.

Delivering these refinements will move the dashboard from partial to full compliance against the legacy COBOL expectations while preserving modernization benefits.

