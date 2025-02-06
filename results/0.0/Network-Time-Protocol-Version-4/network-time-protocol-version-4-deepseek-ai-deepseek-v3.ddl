ntp_v4 {
  header {
    leap_indicator: uint(2);
    version_number: uint(3);
    mode: uint(3);
    stratum: uint(8);
    poll: uint(8);
    precision: uint(8);
    root_delay: uint(32);
    root_dispersion: uint(32);
    reference_id: uint(32);
    reference_timestamp: uint(64);
    originate_timestamp: uint(64);
    receive_timestamp: uint(64);
    transmit_timestamp: uint(64);
  }
  extension_fields: optional {
    length: uint(16);
    data: bytes(length);
  }
  key_identifier: optional uint(32);
  mac: optional bytes(16);
}