structure bitcoin_transaction {
  uint32 version;
  vector<transaction_input> vin;
  vector<transaction_output> vout;
  uint32 locktime;
}

structure transaction_input {
  bytes32 prev_out_hash;
  uint32 prev_out_index;
  bytes scriptSig;
  uint32 sequence;
}

structure transaction_output {
  uint64 value;
  bytes scriptPubKey;
}
