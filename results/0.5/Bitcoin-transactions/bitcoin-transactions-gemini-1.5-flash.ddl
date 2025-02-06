structure bitcoin_transaction {
    uint32 version;
    array transaction_input inputs;
    array transaction_output outputs;
    uint32 locktime;
}

structure transaction_input {
    bytes32 prev_tx_hash;
    uint32 prev_tx_index;
    bytes scriptSig;
    uint32 sequence;
}

structure transaction_output {
    uint64 value;
    bytes scriptPubKey;
}
