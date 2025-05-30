type ntp_packet {
  leap: uint1;
  version: uint3;
  mode: uint3;
  stratum: uint8;
  poll: uint8;
  precision: int8;
  root_delay: uint32;
  root_dispersion: uint32;
  reference_id: uint32;
  reference_timestamp: uint64;
  originate_timestamp: uint64;
  receive_timestamp: uint64;
  transmit_timestamp: uint64;
}
