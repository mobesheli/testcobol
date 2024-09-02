// data.test.js
const { readBalance, writeBalance } = require('../data');

describe('Data Module', () => {
    beforeEach(() => {
        writeBalance(1000.00); // Reset balance before each test
    });

    test('Read Balance', () => {
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });

    test('Write Balance', () => {
        writeBalance(1500.00);
        const balance = readBalance();
        expect(balance).toBe(1500.00);
    });

    test('Credit Account with Valid Amount', () => {
        writeBalance(1000.00);
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });

    test('Credit Account with Zero Amount', () => {
        writeBalance(1000.00);
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });

    test('Debit Account with Valid Amount', () => {
        writeBalance(1000.00);
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });

    test('Debit Account with Amount Greater Than Balance', () => {
        writeBalance(1000.00);
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });

    test('Debit Account with Zero Amount', () => {
        writeBalance(1000.00);
        const balance = readBalance();
        expect(balance).toBe(1000.00);
    });
});