type ntp_packet {
  leap: uint:1;
  version: uint:3;
  mode: uint:3;
  stratum: uint8;
  poll: int8;
  precision: int8;
  root_delay: uint32;
  root_dispersion: uint32;
  reference_id: uint32;
  reference_timestamp: uint64;
  originate_timestamp: uint64;
  receive_timestamp: uint64;
  transmit_timestamp: uint64;
  // Optional extensions can be added here as needed.  RFC 5905 allows for them, but does not define a specific format.
}

