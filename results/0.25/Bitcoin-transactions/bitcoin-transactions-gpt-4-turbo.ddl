module Bitcoin.Transactions {
  import DAEDALUS::Bit;

  type uint32 = Bits32;
  type uint64 = Bits64;

  type VarInt = union {
    case 0..252: uint8;
    case 253: struct {
      marker: uint8;
      value: Bits16;
    };
    case 254: struct {
      marker: uint8;
      value: Bits32;
    };
    case 255: struct {
      marker: uint8;
      value: Bits64;
    };
  }

  type TxIn = struct {
    previous_output: struct {
      hash: array[uint8, 32];
      index: uint32;
    };
    script_length: VarInt;
    script: array[uint8, script_length];
    sequence: uint32;
  }

  type TxOut = struct {
    value: uint64;
    pk_script_length: VarInt;
    pk_script: array[uint8, pk_script_length];
  }

  type Transaction = struct {
    version: uint32;
    in_count: VarInt;
    inputs: array[TxIn, in_count];
    out_count: VarInt;
    outputs: array[TxOut, out_count];
    lock_time: uint32;
  }

  type Block = struct {
    magic: uint32;
    block_size: uint32;
    header: struct {
      version: uint32;
      prev_block: array[uint8, 32];
      merkle_root: array[uint8, 32];
      timestamp: uint32;
      bits: uint32;
      nonce: uint32;
    };
    transaction_count: VarInt;
    transactions: array[Transaction, transaction_count];
  }

  type BitcoinFile = struct {
    blocks: array[Block];
  }
}