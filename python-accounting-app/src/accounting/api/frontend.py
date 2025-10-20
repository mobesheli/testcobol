"""
Frontend web interface for the accounting system.

Provides an interactive dashboard that communicates with the REST API.
Designed to give a lightweight, enterprise-style experience without external assets.
"""

from fastapi import APIRouter
from fastapi.responses import HTMLResponse

router = APIRouter(tags=["frontend"])


FRONTEND_HTML = """<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Account Management Dashboard</title>
    <style>
      :root {
        color-scheme: light dark;
        --surface: rgba(255, 255, 255, 0.85);
        --surface-dark: rgba(15, 23, 42, 0.92);
        --primary: #2563eb;
        --primary-dark: #1d4ed8;
        --success: #22c55e;
        --danger: #ef4444;
        --text: #0f172a;
        --text-muted: #475569;
        --border: rgba(148, 163, 184, 0.4);
      }
      @media (prefers-color-scheme: dark) {
        :root {
          --surface: rgba(15, 23, 42, 0.92);
          --surface-dark: rgba(15, 23, 42, 0.92);
          --text: #e2e8f0;
          --text-muted: #94a3b8;
          --border: rgba(71, 85, 105, 0.6);
        }
      }
      *,
      *::before,
      *::after {
        box-sizing: border-box;
      }
      body {
        margin: 0;
        min-height: 100vh;
        font-family: "Segoe UI", "Inter", system-ui, -apple-system, sans-serif;
        background: radial-gradient(circle at 20% 20%, #f1f5f9, #cbd5f5);
        color: var(--text);
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 48px 16px;
      }
      .app-shell {
        width: min(1080px, 100%);
        backdrop-filter: blur(18px);
        background: var(--surface);
        border-radius: 18px;
        border: 1px solid var(--border);
        box-shadow: 0 25px 60px rgba(15, 23, 42, 0.15);
        padding: 36px;
      }
      header {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        align-items: baseline;
        justify-content: space-between;
        margin-bottom: 32px;
      }
      header h1 {
        margin: 0;
        font-size: clamp(26px, 3vw, 32px);
        letter-spacing: -0.015em;
      }
      header p {
        margin: 0;
        color: var(--text-muted);
      }
      .grid {
        display: grid;
        gap: 24px;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        margin-bottom: 32px;
      }
      .card {
        border: 1px solid var(--border);
        border-radius: 16px;
        padding: 24px;
        background: rgba(255, 255, 255, 0.45);
        backdrop-filter: blur(12px);
        transition: transform 120ms ease, box-shadow 120ms ease;
      }
      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 18px 40px rgba(148, 163, 184, 0.22);
      }
      .balance-value {
        font-size: clamp(32px, 4vw, 44px);
        font-weight: 600;
        margin: 16px 0 0;
        color: var(--primary-dark);
      }
      .label {
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: var(--text-muted);
      }
      form {
        display: flex;
        flex-direction: column;
        gap: 18px;
        margin-top: 12px;
      }
      .input-group {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }
      label {
        font-weight: 600;
        font-size: 13px;
        letter-spacing: 0.03em;
        color: var(--text-muted);
      }
      input[type="number"] {
        width: 100%;
        padding: 12px 14px;
        border-radius: 10px;
        border: 1px solid var(--border);
        font-size: 15px;
        transition: border-color 160ms ease, box-shadow 160ms ease;
        background: rgba(255, 255, 255, 0.9);
      }
      input[type="number"]:focus {
        outline: none;
        border-color: var(--primary);
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.18);
      }
      button {
        border: none;
        padding: 12px 16px;
        border-radius: 10px;
        font-weight: 600;
        font-size: 15px;
        letter-spacing: 0.03em;
        cursor: pointer;
        transition: transform 120ms ease, box-shadow 120ms ease, background 160ms ease;
      }
      button.primary {
        background: linear-gradient(135deg, var(--primary), #60a5fa);
        color: white;
        box-shadow: 0 8px 20px rgba(37, 99, 235, 0.2);
      }
      button.primary:hover:not([disabled]) {
        transform: translateY(-1px);
        box-shadow: 0 12px 28px rgba(37, 99, 235, 0.28);
      }
      button.secondary {
        background: rgba(37, 99, 235, 0.08);
        color: var(--primary-dark);
      }
      button[disabled] {
        opacity: 0.6;
        cursor: not-allowed;
      }
      .transactions {
        border-radius: 18px;
        border: 1px solid var(--border);
        overflow: hidden;
        background: rgba(255, 255, 255, 0.65);
        backdrop-filter: blur(10px);
      }
      table {
        width: 100%;
        border-collapse: collapse;
      }
      thead {
        background: rgba(37, 99, 235, 0.08);
        text-transform: uppercase;
        font-size: 12px;
        letter-spacing: 0.08em;
        color: var(--text-muted);
      }
      th,
      td {
        padding: 14px 18px;
        border-bottom: 1px solid rgba(226, 232, 240, 0.7);
        text-align: left;
      }
      tbody tr:last-child td {
        border-bottom: none;
      }
      .status-pill {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.06em;
      }
      .status-pill.success {
        background: rgba(34, 197, 94, 0.18);
        color: #15803d;
      }
      .status-pill.error {
        background: rgba(239, 68, 68, 0.18);
        color: #b91c1c;
      }
      .toast {
        position: fixed;
        top: 24px;
        right: 24px;
        padding: 16px 20px;
        border-radius: 12px;
        background: var(--surface-dark);
        color: #f8fafc;
        box-shadow: 0 18px 36px rgba(15, 23, 42, 0.25);
        opacity: 0;
        transform: translateY(-12px);
        pointer-events: none;
        transition: opacity 200ms ease, transform 200ms ease;
        max-width: min(320px, 80vw);
        line-height: 1.4;
      }
      .toast.visible {
        opacity: 1;
        transform: translateY(0);
      }
      .toast.positive {
        border-left: 4px solid var(--success);
      }
      .toast.negative {
        border-left: 4px solid var(--danger);
      }
      .refresh-button {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        margin-top: 12px;
        font-size: 14px;
        color: var(--primary-dark);
        cursor: pointer;
      }
      .refresh-button svg {
        width: 14px;
        height: 14px;
      }
      @media (max-width: 720px) {
        body {
          padding: 24px 12px;
        }
        .app-shell {
          padding: 28px 20px;
        }
        th,
        td {
          padding: 12px 14px;
        }
      }
    </style>
  </head>
  <body>
    <div class="app-shell">
      <header>
        <div>
          <h1>Account Management Dashboard</h1>
          <p>Monitor balances, process credits or debits, and audit transaction history in real time.</p>
        </div>
        <button class="secondary" id="refresh-all">Refresh Data</button>
      </header>

      <section class="grid">
        <article class="card">
          <span class="label">Current Balance</span>
          <div class="balance-value" id="balance-display">$0.00</div>
          <div class="refresh-button" id="refresh-balance">
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none">
              <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"
                d="M19.5 12a7.5 7.5 0 10-1.846 4.916" />
              <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"
                d="M19.5 7.5v4.5 h-4.5" />
            </svg>
            Refresh balance
          </div>
        </article>

        <article class="card">
          <span class="label">Credit Account</span>
          <form id="credit-form">
            <div class="input-group">
              <label for="credit-amount">Amount (USD)</label>
              <input id="credit-amount" name="credit-amount" type="number" placeholder="e.g. 150.00" step="0.01" min="0.01" max="999999.99" required />
            </div>
            <button class="primary" type="submit" id="credit-submit">Post Credit</button>
          </form>
        </article>

        <article class="card">
          <span class="label">Debit Account</span>
          <form id="debit-form">
            <div class="input-group">
              <label for="debit-amount">Amount (USD)</label>
              <input id="debit-amount" name="debit-amount" type="number" placeholder="e.g. 75.00" step="0.01" min="0.01" max="999999.99" required />
            </div>
            <button class="primary" type="submit" id="debit-submit">Post Debit</button>
          </form>
        </article>
      </section>

      <section class="transactions">
        <table>
          <thead>
            <tr>
              <th>Timestamp</th>
              <th>Type</th>
              <th>Amount</th>
              <th>Balance After</th>
              <th>Status</th>
              <th>Notes</th>
            </tr>
          </thead>
          <tbody id="transactions-body">
            <tr>
              <td colspan="6">No transactions loaded yet.</td>
            </tr>
          </tbody>
        </table>
      </section>
    </div>

    <div class="toast" id="toast"></div>

    <script>
      const API_BASE = "/api/v1/account";

      const balanceDisplay = document.getElementById("balance-display");
      const toast = document.getElementById("toast");
      const transactionsBody = document.getElementById("transactions-body");
      const creditForm = document.getElementById("credit-form");
      const debitForm = document.getElementById("debit-form");
      const creditSubmit = document.getElementById("credit-submit");
      const debitSubmit = document.getElementById("debit-submit");

      const showToast = (message, type = "positive") => {
        toast.textContent = message;
        toast.className = `toast visible ${type === "positive" ? "positive" : "negative"}`;
        setTimeout(() => {
          toast.classList.remove("visible");
        }, 3400);
      };

      const dollarsToCents = (value) => {
        if (!value) {
          return null;
        }
        return Math.round(parseFloat(value) * 100);
      };

      const formatCurrency = (cents) => {
        return new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
        }).format(cents / 100);
      };

      const fetchBalance = async () => {
        try {
          const response = await fetch(`${API_BASE}/balance`);
          if (!response.ok) {
            throw new Error("Unable to fetch balance.");
          }
          const data = await response.json();
          balanceDisplay.textContent = data.balance_display ?? formatCurrency(data.balance_cents);
        } catch (error) {
          showToast(error.message, "negative");
        }
      };

      const fetchTransactions = async () => {
        try {
          const response = await fetch(`${API_BASE}/transactions?limit=50`);
          if (!response.ok) {
            throw new Error("Unable to load transactions.");
          }
          const data = await response.json();
          const transactions = data.transactions ?? [];

          if (!transactions.length) {
            transactionsBody.innerHTML = '<tr><td colspan="6">No transactions recorded yet.</td></tr>';
            return;
          }

          transactionsBody.innerHTML = transactions
            .map((transaction) => {
              const statusClass = transaction.success ? "success" : "error";
              const notes = transaction.error_message ?? "";
              const amount = formatCurrency(transaction.amount_cents);
              const balanceAfter = formatCurrency(transaction.balance_after_cents);
              const typeIcon =
                transaction.type === "CREDIT"
                  ? "⬆"
                  : transaction.type === "DEBIT"
                  ? "⬇"
                  : "ℹ";

              return `
                <tr>
                  <td>${new Date(transaction.timestamp).toLocaleString()}</td>
                  <td>${typeIcon}&nbsp;${transaction.type}</td>
                  <td>${amount}</td>
                  <td>${balanceAfter}</td>
                  <td><span class="status-pill ${statusClass}">${transaction.success ? "Success" : "Declined"}</span></td>
                  <td>${notes}</td>
                </tr>
              `;
            })
            .join("");
        } catch (error) {
          showToast(error.message, "negative");
        }
      };

      const submitTransaction = async (formElement, submitButton, endpoint) => {
        const amountField = formElement.querySelector("input[type='number']");
        const amountValue = amountField.value;
        const amountCents = dollarsToCents(amountValue);

        if (!amountCents || amountCents <= 0) {
          showToast("Please provide a valid amount greater than zero.", "negative");
          return;
        }

        submitButton.disabled = true;
        submitButton.textContent = "Processing...";

        try {
          const response = await fetch(`${API_BASE}/${endpoint}`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ amount_cents: amountCents }),
          });

          const data = await response.json();
          if (!response.ok) {
            const detail = data.detail ?? "Transaction failed.";
            throw new Error(detail);
          }

          if (!data.success) {
            showToast(data.message ?? "Transaction declined.", "negative");
          } else {
            showToast(data.message ?? "Transaction processed.", "positive");
          }

          formElement.reset();
          await fetchBalance();
          await fetchTransactions();
        } catch (error) {
          showToast(error.message, "negative");
        } finally {
          submitButton.disabled = false;
          submitButton.textContent = endpoint === "credit" ? "Post Credit" : "Post Debit";
        }
      };

      creditForm.addEventListener("submit", async (event) => {
        event.preventDefault();
        await submitTransaction(creditForm, creditSubmit, "credit");
      });

      debitForm.addEventListener("submit", async (event) => {
        event.preventDefault();
        await submitTransaction(debitForm, debitSubmit, "debit");
      });

      document.getElementById("refresh-all").addEventListener("click", async () => {
        await Promise.all([fetchBalance(), fetchTransactions()]);
        showToast("Dashboard refreshed.", "positive");
      });

      document.getElementById("refresh-balance").addEventListener("click", async () => {
        await fetchBalance();
        showToast("Balance updated.", "positive");
      });

      // Initial load
      fetchBalance();
      fetchTransactions();
    </script>
  </body>
</html>
"""


@router.get("/dashboard", response_class=HTMLResponse, include_in_schema=False)
async def accounting_dashboard() -> HTMLResponse:
    """Serve the interactive dashboard."""
    return HTMLResponse(content=FRONTEND_HTML)
