domain BitcoinTransactions {
  // Import required types and constants
  import types: Integer, String, Bytes, DateTime, Boolean
  import constants: MAX_TRANSACTION_SIZE

  // Define the structure of a Bitcoin transaction
  structure Transaction {
    // Transaction version
    field version: Integer(4)

    // Input count
    field input_count: Integer(1..MAX_TRANSACTION_SIZE)

    // Transaction inputs
    field inputs: array(TransactionInput, input_count)

    // Output count
    field output_count: Integer(1..MAX_TRANSACTION_SIZE)

    // Transaction outputs
    field outputs: array(TransactionOutput, output_count)

    // Lock time
    field lock_time: Integer(4)
  }

  // Define the structure of a transaction input
  structure TransactionInput {
    // Previous transaction hash
    field prev_transaction_hash: Bytes(32)

    // Previous transaction index
    field prev_transaction_index: Integer(4)

    // Script length
    field script_length: Integer(1..MAX_TRANSACTION_SIZE)

    // Script
    field script: Bytes(script_length)

    // Sequence number
    field sequence_number: Integer(4)
  }

  // Define the structure of a transaction output
  structure TransactionOutput {
    // Value
    field value: Integer(8)

    // Script length
    field script_length: Integer(1..MAX_TRANSACTION_SIZE)

    // Script
    field script: Bytes(script_length)
  }

  // Define the top-level context
  context Top {
    // Transaction count
    field transaction_count: Integer(1..MAX_TRANSACTION_SIZE)

    // Transactions
    field transactions: array(Transaction, transaction_count)
  }
}