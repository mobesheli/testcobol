// index.js
const readline = require('readline');
const { viewBalance, creditAccount, debitAccount } = require('./operations');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function displayMenu() {
    console.log('--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
    rl.question('Enter your choice (1-4): ', handleUserChoice);
}

function handleUserChoice(choice) {
    switch (choice) {
        case '1':
            viewBalance();
            break;
        case '2':
            rl.question('Enter credit amount: ', (amount) => {
                creditAccount(parseFloat(amount));
                displayMenu();
            });
            return;
        case '3':
            rl.question('Enter debit amount: ', (amount) => {
                debitAccount(parseFloat(amount));
                displayMenu();
            });
            return;
        case '4':
            console.log('Exiting the program. Goodbye!');
            rl.close();
            return;
        default:
            console.log('Invalid choice, please select 1-4.');
    }
    displayMenu();
}

displayMenu();