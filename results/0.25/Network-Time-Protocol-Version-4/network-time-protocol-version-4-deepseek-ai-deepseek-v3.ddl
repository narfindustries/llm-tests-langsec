network_time_protocol_v4 {
  li: uint 2;
  vn: uint 3;
  mode: uint 3;
  stratum: uint 8;
  poll: uint 8;
  precision: uint 8;
  root_delay: uint 32;
  root_dispersion: uint 32;
  reference_id: uint 32;
  reference_timestamp: uint 64;
  origin_timestamp: uint 64;
  receive_timestamp: uint 64;
  transmit_timestamp: uint 64;
  key_identifier?: uint 32;
  message_digest?: uint 128;
}