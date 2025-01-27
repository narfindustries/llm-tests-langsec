specification BitcoinTransactions {
    lexer {
        digit = ['0'-'9']
        hex = ['0'-'9' 'a'-'f' 'A'-'F']
        ws = ' ' | '\t' | '\n' | '\r'
        ignored = ws+
    }

    token Amount = digit+
    token TxHash = hex{64}
    token Address = ['1' '3' 'b' 'B'][a-zA-Z0-9]{25,34}

    Transaction = {
        inputs: [Input+]
        outputs: [Output+]
        hash: TxHash
        timestamp: Amount
    }

    Input = {
        prevTxHash: TxHash
        outputIndex: Amount
        signature: TxHash
    }

    Output = {
        amount: Amount
        address: Address
    }

    BitcoinBlock = {
        transactions: [Transaction+]
        blockHeight: Amount
        blockHash: TxHash
        previousBlockHash: TxHash
        timestamp: Amount
    }

    main = BitcoinBlock
}