domain bitcoin_transactions {
  sequence Transaction {
    uint32_t transaction_version: 4;
    varuint32_t num_inputs: 1;
    sequence inputs: num_inputs {
      bytes prev TxHash: 32;
      uint32_t prev_index: 4;
      bytes scriptSIG: *;
      uint32_t sequence: 4;
    }
    varuint32_t num_outputs: 1;
    sequence outputs: num_outputs {
      uint64_t value: 8;
      bytes scriptPubKey: *;
    }
    uint32_t lock_time: 4;
  }

  sequence BlockHeader {
    uint32_t block_version: 4;
    bytes prev_block_hash: 32;
    bytes merkle_root: 32;
    uint32_t timestamp: 4;
    uint32_t target: 4;
    uint32_t nonce: 4;
  }

  sequence Block {
    BlockHeader block_header: 1;
    varuint32_t num_transactions: 1;
    sequence transactions: num_transactions {
      Transaction transaction: 1;
    }
  }
}