module Bitcoin.Transactions {

  import DAEDALUS::Bit;
  import DAEDALUS::Num;

  type uint32 = Num::uint32_le;
  type uint64 = Num::uint64_le;
  type var_int = Num::VarInt;

  type tx_in = struct {
    previous_output: struct {
      hash: Bit::bytes<32>;
      index: uint32;
    };
    script_length: var_int;
    signature_script: Bit::bytes<self.script_length.value>;
    sequence: uint32;
  };

  type tx_out = struct {
    value: uint64;
    pk_script_length: var_int;
    pk_script: Bit::bytes<self.pk_script_length.value>;
  };

  type transaction = struct {
    version: uint32;
    in_count: var_int;
    inputs: array<tx_in, self.in_count.value>;
    out_count: var_int;
    outputs: array<tx_out, self.out_count.value>;
    lock_time: uint32;
  };

  type bitcoin_transactions = struct {
    count: var_int;
    transactions: array<transaction, self.count.value>;
  };

  let main = bitcoin_transactions;
}