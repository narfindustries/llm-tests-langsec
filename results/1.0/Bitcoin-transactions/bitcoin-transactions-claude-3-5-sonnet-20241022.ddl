def Bitcoin = struct {
  version: U32BE;
  has_witness: U8;
  witness_flag: U8 when has_witness == 0x00;
  tx_in_count: VarInt;
  tx_in: TxIn[tx_in_count];
  tx_out_count: VarInt;
  tx_out: TxOut[tx_out_count];
  witness_data: WitnessData[tx_in_count] when has_witness == 0x00;
  lock_time: U32BE
}

def TxIn = struct {
  previous_tx_hash: U8[32];
  previous_output_index: U32BE;
  script_length: VarInt;
  signature_script: U8[script_length];
  sequence: U32BE
}

def TxOut = struct {
  value: U64BE;
  script_length: VarInt;
  output_script: U8[script_length]
}

def WitnessData = struct {
  witness_count: VarInt;
  witness_items: WitnessItem[witness_count]
}

def WitnessItem = struct {
  item_length: VarInt;
  witness_item: U8[item_length]
}

def VarInt = struct {
  flag: U8;
  value: select flag {
    case flag if flag <= 0xfc => flag;
    case 0xfd => U16BE;
    case 0xfe => U32BE;
    case 0xff => U64BE
  }
}

def P2PKH_Script = struct {
  op_dup: U8;
  op_hash160: U8;
  pub_key_hash: U8[20];
  op_equalverify: U8;
  op_checksig: U8
}

def P2SH_Script = struct {
  op_hash160: U8;
  script_hash: U8[20];
  op_equal: U8
}

def OP_RETURN_Script = struct {
  op_return: U8;
  data_length: VarInt;
  data: U8[data_length]
}