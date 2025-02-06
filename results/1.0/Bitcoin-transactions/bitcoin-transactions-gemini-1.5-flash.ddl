module BitcoinTransaction

import std::integer
import std::bytes
import std::vector

record Transaction {
  version: integer::uint32,
  inputs: vector[TxIn],
  outputs: vector[TxOut],
  locktime: integer::uint32,
  witness: optional[Witness]
}

record TxIn {
  prev_tx_hash: bytes::bytes32,
  prev_out_index: integer::uint32,
  script_sig: bytes::bytes,
  sequence: integer::uint32
}

record TxOut {
  value: integer::uint64,
  script_pubkey: bytes::bytes
}

record Witness {
  witnesses: vector[vector[bytes::bytes]]
}

function varint(n:integer):bytes = 
  if n < 0xfd then integer::to_bytes_le(n,1)
  else if n <= 0xffff then concat[bytes::from_int(0xfd,1), integer::to_bytes_le(n,2)]
  else if n <= 0xffffffff then concat[bytes::from_int(0xfe,1), integer::to_bytes_le(n,4)]
  else concat[bytes::from_int(0xff,1), integer::to_bytes_le(n,8)]


function serialize(tx:Transaction):bytes = {
  concat [
    integer::to_bytes_le(tx.version, 4),
    varint(length(tx.inputs)),
    concat (map (\i -> concat [i.prev_tx_hash, integer::to_bytes_le(i.prev_out_index, 4), varint (length (i.script_sig)), i.script_sig, integer::to_bytes_le(i.sequence, 4)], tx.inputs)),
    varint(length(tx.outputs)),
    concat (map (\o -> concat [integer::to_bytes_le(o.value, 8), varint (length (o.script_pubkey)), o.script_pubkey], tx.outputs)),
    integer::to_bytes_le(tx.locktime, 4),
    case tx.witness of
      Nothing -> bytes::empty
      Just w -> concat [varint (length (w.witnesses)), concat (map (\v -> concat (map (\item -> concat [varint (length item), item], v)), w.witnesses))]
  ]
}
