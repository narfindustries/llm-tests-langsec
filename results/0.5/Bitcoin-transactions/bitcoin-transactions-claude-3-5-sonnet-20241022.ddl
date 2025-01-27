specification BitcoinTransactions;

type Amount = nat64;
type Address = string;
type TransactionID = string;
type Timestamp = nat64;

type Transaction = {
    txID: TransactionID,
    timestamp: Timestamp,
    sender: Address,
    receiver: Address,
    amount: Amount,
    fee: Amount
}

type Block = {
    blockNumber: nat64,
    timestamp: Timestamp,
    transactions: Transaction*
}

type Blockchain = Block*

@entry
grammar BitcoinParser {
    Blockchain = Block*

    Block = {
        blockNumber: UInt
        NEWLINE
        timestamp: UInt
        NEWLINE
        transactions: Transaction*
    }

    Transaction = {
        txID: TxID
        WHITESPACE
        timestamp: UInt
        WHITESPACE
        sender: Addr
        WHITESPACE
        receiver: Addr
        WHITESPACE
        amount: UInt
        WHITESPACE
        fee: UInt
        NEWLINE
    }

    TxID = /[0-9a-f]{64}/
    Addr = /[13][a-km-zA-HJ-NP-Z1-9]{25,34}/
    UInt = /[0-9]+/
    WHITESPACE = /[ \t]+/
    NEWLINE = /[\n\r]+/
}

@test BitcoinParser {
    "1\n1634567890\ntx123 1634567890 1ABC123def 3XYZ789abc 100 5\n"
    =>
    [
        {
            blockNumber: 1,
            timestamp: 1634567890,
            transactions: [
                {
                    txID: "tx123",
                    timestamp: 1634567890,
                    sender: "1ABC123def",
                    receiver: "3XYZ789abc",
                    amount: 100,
                    fee: 5
                }
            ]
        }
    ]
}