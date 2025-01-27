domain bitcoin_transactions {
  type transaction {
    bit_field {
      name: "version"
      length: 32
      type: uint
    }
    var_length_string {
      name: "tx_in_count"
      type: uint
    }
    repeated {
      name: "tx_in"
      type: tx_in
      length: "tx_in_count"
    }
    var_length_string {
      name: "tx_out_count"
      type: uint
    }
    repeated {
      name: "tx_out"
      type: tx_out
      length: "tx_out_count"
    }
    bit_field {
      name: "lock_time"
      length: 32
      type: uint
    }
  }

  type tx_in {
    bytes {
      name: "tx_id"
      length: 32
    }
    uint {
      name: "tx_index"
      length: 32
    }
    bytes {
      name: "script_sig"
      length: var
    }
    bytes {
      name: "sequence"
      length: 32
    }
  }

  type tx_out {
    uint {
      name: "value"
      length: 64
    }
    bytes {
      name: "script_pub_key"
      length: var
    }
  }

  type block_header {
    bit_field {
      name: "version"
      length: 32
      type: uint
    }
    bytes {
      name: "prev_block"
      length: 32
    }
    bytes {
      name: "merkle_root"
      length: 32
    }
    uint {
      name: "timestamp"
      length: 32
    }
    uint {
      name: "bits"
      length: 32
    }
    uint {
      name: "nonce"
      length: 32
    }
  }

  type block {
    bit_field {
      name: "header"
      type: block_header
    }
    var_length_string {
      name: "transaction_count"
      type: uint
    }
    repeated {
      name: "transactions"
      type: transaction
      length: "transaction_count"
    }
  }

  root_type block
}