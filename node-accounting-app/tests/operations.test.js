// operations.test.js
const { readBalance, writeBalance } = require('../data');
const { viewBalance, creditAccount, debitAccount } = require('../operations');

jest.mock('../data', () => ({
    readBalance: jest.fn(),
    writeBalance: jest.fn()
}));

describe('Account Management System', () => {
    beforeEach(() => {
        readBalance.mockClear();
        writeBalance.mockClear();
    });

    test('View Current Balance', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        viewBalance();
        expect(console.log).toHaveBeenCalledWith('Current balance: 1000.00');
    });

    test('Credit Account with Valid Amount', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        creditAccount(100.00);
        expect(writeBalance).toHaveBeenCalledWith(1100.00);
        expect(console.log).toHaveBeenCalledWith('Amount credited. New balance: 1100.00');
    });

    test('Credit Account with Zero Amount', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        creditAccount(0.00);
        expect(writeBalance).toHaveBeenCalledWith(1000.00);
        expect(console.log).toHaveBeenCalledWith('Amount credited. New balance: 1000.00');
    });

    test('Debit Account with Valid Amount', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        debitAccount(50.00);
        expect(writeBalance).toHaveBeenCalledWith(950.00);
        expect(console.log).toHaveBeenCalledWith('Amount debited. New balance: 950.00');
    });

    test('Debit Account with Amount Greater Than Balance', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        debitAccount(2000.00);
        expect(writeBalance).not.toHaveBeenCalled();
        expect(console.log).toHaveBeenCalledWith('Insufficient funds for this debit.');
    });

    test('Debit Account with Zero Amount', () => {
        readBalance.mockReturnValue(1000.00);
        console.log = jest.fn();
        debitAccount(0.00);
        expect(writeBalance).toHaveBeenCalledWith(1000.00);
        expect(console.log).toHaveBeenCalledWith('Amount debited. New balance: 1000.00');
    });
});