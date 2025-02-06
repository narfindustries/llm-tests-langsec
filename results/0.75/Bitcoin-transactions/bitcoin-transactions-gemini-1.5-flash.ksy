types:
  uint32:
    type: seq
    size: 4
    endian: le
  varint:
    type: seq
    size: 1
    read: varint
  var_bytes:
    type: seq
    read: varint
  txin:
    seq:
      - prev_out:
          hash:
            type: bytes
            size: 32
          index:
            type: uint32
      - script_sig:
        type: var_bytes
      - sequence:
        type: uint32
  txout:
    seq:
      - value:
        type: uint64
      - pk_script:
        type: var_bytes
  tx:
    seq:
      - version:
        type: uint32
      - in_count:
        type: varint
      - ins:
        type: txin
        repeat: expr
        repeat-expr: in_count
      - out_count:
        type: varint
      - outs:
        type: txout
        repeat: expr
        repeat-expr: out_count
      - lock_time:
        type: uint32

