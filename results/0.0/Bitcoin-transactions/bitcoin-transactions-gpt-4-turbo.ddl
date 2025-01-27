module Bitcoin.Transactions {
  import DAEDALUS::Bit;
  import DAEDALUS::Num;

  type uint32 = Num::uint(32, Num::BE);
  type uint64 = Num::uint(64, Num::BE);
  type var_int = Num::varint();

  type tx_in = struct {
    previous_output : struct {
      hash  : Bit::bytes(32);
      index : uint32;
    };
    script_length : var_int;
    script : Bit::bytes(script_length);
    sequence : uint32;
  };

  type tx_out = struct {
    value : uint64;
    script_length : var_int;
    script : Bit::bytes(script_length);
  };

  type transaction = struct {
    version : uint32;
    in_count : var_int;
    inputs : [tx_in](in_count);
    out_count : var_int;
    outputs : [tx_out](out_count);
    lock_time : uint32;
  };

  type block = struct {
    magic_number : uint32;
    block_size : uint32;
    version : uint32;
    prev_block : Bit::bytes(32);
    merkle_root : Bit::bytes(32);
    timestamp : uint32;
    bits : uint32;
    nonce : uint32;
    txn_count : var_int;
    txns : [transaction](txn_count);
  };

  type file = [block](_);
}