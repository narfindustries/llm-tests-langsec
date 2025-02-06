format binary {
  transaction: transaction
}

transaction {
  tx_version: uint32 { byte_order: big_endian }
  tx_in_count: varint
  tx_in: transaction_input[tx_in_count]
  tx_out_count: varint
  tx_out: transaction_output[tx_out_count]
  tx_lock_time: uint32 { byte_order: little_endian }
}

transaction_input {
  tx_in_prev_output_hash: byte[32]
  tx_in_prev_output_index: uint32 { byte_order: little_endian }
  tx_in_script_sig: byte[]
  tx_in_sequence: uint32 { byte_order: little_endian }
}

transaction_output {
  tx_out_value: uint64 { byte_order: little_endian }
  tx_out_script_pub_key: byte[]
}

block {
  block_header: block_header
  tx_count: varint
  tx: transaction[tx_count]
}

block_header {
  block_version: uint32 { byte_order: little_endian }
  block_prev_block_hash: byte[32]
  block_merkle_root: byte[32]
  block_timestamp: uint32 { byte_order: little_endian }
  block_target: uint32 { byte_order: little_endian }
  block_nonce: uint32 { byte_order: little_endian }
}

network_message {
  choice {
    version: version_message
    verack: verack_message
    getaddr: getaddr_message
    addr: addr_message
    inv: inv_message
    getdata: getdata_message
    getblocks: getblocks_message
    getheaders: getheaders_message
    tx: transaction
    block: block
    headers: headers_message
    reject: reject_message
    ping: ping_message
    pong: pong_message
  }
}

version_message {
  version: uint32 { byte_order: little_endian }
  services: uint64 { byte_order: little_endian }
  timestamp: uint64 { byte_order: little_endian }
  addr_recv: net_address
  addr_from: net_address
  nonce: uint64 { byte_order: little_endian }
  user_agent: byte[]
  start_height: uint32 { byte_order: little_endian }
}

verack_message {
}

getaddr_message {
}

addr_message {
  addr_count: varint
  addr: net_address[addr_count]
}

inv_message {
  inv_count: varint
  inv: inventory[inv_count]
}

getdata_message {
  inv_count: varint
  inv: inventory[inv_count]
}

getblocks_message {
  version: uint32 { byte_order: little_endian }
  block_locator_hash_count: varint
  block_locator_hash: byte[32][block_locator_hash_count]
  hash_stop: byte[32]
}

getheaders_message {
  version: uint32 { byte_order: little_endian }
  block_locator_hash_count: varint
  block_locator_hash: byte[32][block_locator_hash_count]
  hash_stop: byte[32]
}

headers_message {
  header_count: varint
  header: block_header[header_count]
}

reject_message {
  message: byte[]
  cdata: byte[]
  reason: uint8
}

ping_message {
  nonce: uint64 { byte_order: little_endian }
}

pong_message {
  nonce: uint64 { byte_order: little_endian }
}

inventory {
  type: uint32 { byte_order: little_endian }
  hash: byte[32]
}

net_address {
  timestamp: uint32 { byte_order: little_endian }
  services: uint64 { byte_order: little_endian }
  addr: byte[26]
  port: uint16 { byte_order: big_endian }
}

script {
  op_code: uint8
  data: byte[]
}