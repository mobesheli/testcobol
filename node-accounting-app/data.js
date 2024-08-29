// data.js
let storageBalance = 1000.00;

function readBalance() {
    return storageBalance;
}

function writeBalance(newBalance) {
    storageBalance = newBalance;
}

module.exports = {
    readBalance,
    writeBalance
};