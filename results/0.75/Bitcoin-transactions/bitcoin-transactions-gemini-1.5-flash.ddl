type BitcoinTransaction = record {
  version: Int32;
  inputs: Seq TransactionInput;
  outputs: Seq TransactionOutput;
  locktime: UInt32;
};

type TransactionInput = record {
  prevTxId: Hash256;
  prevOutIndex: UInt32;
  scriptSig: Bytes;
  sequence: UInt32;
};

type TransactionOutput = record {
  value: UInt64;
  scriptPubKey: Bytes;
};

type Hash256 = Array[32] Byte;
type Bytes = Array[n] Byte; --Variable length byte array

