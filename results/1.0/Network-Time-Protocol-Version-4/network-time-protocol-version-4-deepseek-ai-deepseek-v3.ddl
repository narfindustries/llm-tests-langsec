network_time_protocol_version_4 = {
  li: uint8 where 0 <= . <= 3,
  vn: uint8 where 0 <= . <= 7,
  mode: uint8 where 0 <= . <= 7,
  stratum: uint8,
  poll: uint8,
  precision: int8,
  root_delay: uint32,
  root_dispersion: uint32,
  reference_identifier: uint32,
  reference_timestamp: uint64,
  origin_timestamp: uint64,
  receive_timestamp: uint64,
  transmit_timestamp: uint64,
  key_identifier: optional<uint32>,
  message_digest: optional<uint128>
}