ntpv4 = struct {
  li: uint 2,
  vn: uint 3,
  mode: uint 3,
  stratum: uint 8,
  poll: uint 8,
  precision: int 8,
  root_delay: uint 32,
  root_dispersion: uint 32,
  ref_id: uint 32,
  ref_timestamp: uint 64,
  orig_timestamp: uint 64,
  recv_timestamp: uint 64,
  trans_timestamp: uint 64,
  key_id: uint 32?,
  message_digest: uint 128?
};