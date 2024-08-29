// operations.js
const { readBalance, writeBalance } = require('./data');

function viewBalance() {
    const balance = readBalance();
    console.log(`Current balance: ${balance.toFixed(2)}`);
}

function creditAccount(amount) {
    let balance = readBalance();
    balance += amount;
    writeBalance(balance);
    console.log(`Amount credited. New balance: ${balance.toFixed(2)}`);
}

function debitAccount(amount) {
    let balance = readBalance();
    if (balance >= amount) {
        balance -= amount;
        writeBalance(balance);
        console.log(`Amount debited. New balance: ${balance.toFixed(2)}`);
    } else {
        console.log('Insufficient funds for this debit.');
    }
}

module.exports = {
    viewBalance,
    creditAccount,
    debitAccount
};