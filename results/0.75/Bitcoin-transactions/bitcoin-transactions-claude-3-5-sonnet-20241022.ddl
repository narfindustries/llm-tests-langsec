specification BitcoinTransactions;

type Amount = {
    value: nat
}

type TransactionInput = {
    prev_tx: hexstring,
    index: nat,
    sig_script: hexstring
}

type TransactionOutput = {
    amount: Amount,
    pk_script: hexstring
}

type Transaction = {
    version: nat,
    inputs: [TransactionInput*],
    outputs: [TransactionOutput+],
    lock_time: nat
}

type Block = {
    version: nat,
    prev_block: hexstring,
    merkle_root: hexstring,
    timestamp: nat,
    bits: nat,
    nonce: nat,
    transactions: [Transaction+]
}

grammar BitcoinGrammar {
    BitcoinBlocks = blocks: [Block*]
}

grammar HexGrammar {
    HexDigit = '0'..'9' | 'a'..'f' | 'A'..'F'
    HexString = HexDigit+
}

type hexstring = @HexGrammar::HexString

start BitcoinGrammar::BitcoinBlocks