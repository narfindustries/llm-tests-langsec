type uint32 = Bits32 {
  value = UInt32;
}

type uint64 = Bits64 {
  value = UInt64;
}

type var_int = {
  value = UInt8 : UInt8;
  match value {
    | x if x < 0xfd => value = x
    | 0xfd => value = UInt16
    | 0xfe => value = UInt32
    | 0xff => value = UInt64
  }
}

type tx_in = {
  previous_output = {
    hash = Bytes[32];
    index = uint32;
  }
  script_length = var_int;
  script = Bytes[script_length.value];
  sequence = uint32;
}

type tx_out = {
  value = uint64;
  script_length = var_int;
  script = Bytes[script_length.value];
}

type transaction = {
  version = uint32;
  input_count = var_int;
  inputs = List(tx_in, input_count.value);
  output_count = var_int;
  outputs = List(tx_out, output_count.value);
  lock_time = uint32;
}

type bitcoin_transactions = {
  count = var_int;
  transactions = List(transaction, count.value);
}