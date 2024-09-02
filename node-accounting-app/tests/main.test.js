const readline = require('readline');
const { viewBalance, creditAccount, debitAccount } = require('../operations');
const { handleUserChoice, displayMenu } = require('../main');

jest.mock('readline');
jest.mock('../operations', () => ({
    viewBalance: jest.fn(),
    creditAccount: jest.fn(),
    debitAccount: jest.fn()
}));

describe('Main Program', () => {
    let rl;

    beforeEach(() => {
        rl = {
            question: jest.fn(),
            close: jest.fn()
        };
        readline.createInterface.mockReturnValue(rl);
    });

    test('View Balance', () => {
        handleUserChoice(rl, '1');
        expect(viewBalance).toHaveBeenCalled();
        expect(rl.question).toHaveBeenCalled();
    });

    test('Credit Account with Valid Amount', () => {
        rl.question.mockImplementationOnce((question, callback) => callback('100.00'));
        handleUserChoice(rl, '2');
        expect(rl.question).toHaveBeenCalledWith('Enter credit amount: ', expect.any(Function));
        expect(creditAccount).toHaveBeenCalledWith(100.00);
        expect(rl.question).toHaveBeenCalled();
    });

    test('Debit Account with Valid Amount', () => {
        rl.question.mockImplementationOnce((question, callback) => callback('50.00'));
        handleUserChoice(rl, '3');
        expect(rl.question).toHaveBeenCalledWith('Enter debit amount: ', expect.any(Function));
        expect(debitAccount).toHaveBeenCalledWith(50.00);
        expect(rl.question).toHaveBeenCalled();
    });

    test('Exit Application', () => {
        console.log = jest.fn();
        handleUserChoice(rl, '4');
        expect(rl.close).toHaveBeenCalled();
        expect(console.log).toHaveBeenCalledWith('Exiting the program. Goodbye!');
    });

    test('Invalid Choice', () => {
        console.log = jest.fn();
        handleUserChoice(rl, '5');
        expect(console.log).toHaveBeenCalledWith('Invalid choice, please select 1-4.');
        expect(rl.question).toHaveBeenCalled();
    });
});