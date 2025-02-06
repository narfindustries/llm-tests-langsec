module BitcoinTransaction

import std.integer

record TxIn {
  prev_out : TxOutPoint;
  scriptSig : bytes;
  sequence : uint32;
}

record TxOutPoint {
  hash : bytes[32];
  index : uint32;
}

record TxOut {
  value : uint64;
  scriptPubKey : bytes;
}

record BitcoinTransaction {
  version : int32;
  inputs : [TxIn];
  outputs : [TxOut];
  locktime : uint32;
  witness : Optional [Witness];
}

record Witness {
  stack : [[bytes]];
}
