specification BitcoinTransactions;

type Amount = nat64;
type Address = string;
type TransactionID = string;
type Timestamp = nat64;

type Transaction = {
    txID: TransactionID,
    timestamp: Timestamp,
    sender: Address,
    recipient: Address,
    amount: Amount,
    fee: Amount
}

type Block = {
    transactions: Transaction*,
    blockTimestamp: Timestamp
}

type Blockchain = Block*

@entry
grammar BitcoinGrammar {
    transaction = txID:transactionID WS
                 timestamp:timestamp WS
                 sender:address WS
                 recipient:address WS
                 amount:amount WS
                 fee:amount
                 { Transaction { txID, timestamp, sender, recipient, amount, fee } }

    block = "BLOCK" WS blockTimestamp:timestamp NL
           transactions:transaction+ 
           "END_BLOCK" NL
           { Block { transactions, blockTimestamp } }

    blockchain = blocks:block*
                { Blockchain blocks }

    transactionID = ~"[0-9a-f]{64}" { String.fromCharList(text) }
    timestamp = num:number { Number.fromText(text) }
    address = ~"[13][a-km-zA-HJ-NP-Z1-9]{25,34}" { String.fromCharList(text) }
    amount = num:number { Number.fromText(text) }
    number = ~"[0-9]+" { text }
    
    WS = " "+
    NL = "\n"+
}

@extern
function String.fromCharList(text: char[]): string

@extern
function Number.fromText(text: char[]): nat64