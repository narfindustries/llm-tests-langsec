bitcoin_transaction = {
  version: uint32;
  inputs: var_int input[inputs];
  outputs: var_int output[outputs];
  locktime: uint32;
  witness: optional witness;
};

input = {
  previous_tx_hash: byte[32];
  previous_output_index: uint32;
  script_sig: var_int byte[script_sig];
  sequence: uint32;
};

output = {
  value: int64;
  script_pub_key: var_int byte[script_pub_key];
};

witness = {
  witness_count: var_int witness_data[witness_count];
};

witness_data = {
  data: var_int byte[data];
};

var_int = {
  value: uint8;
  extended: switch(value) {
    case 0xFD: uint16;
    case 0xFE: uint32;
    case 0xFF: uint64;
    default: uint8 if(value < 0xFD);
  };
};