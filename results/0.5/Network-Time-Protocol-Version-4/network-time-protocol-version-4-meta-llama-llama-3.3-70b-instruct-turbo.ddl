domain network_time_protocol_version_4 {
  include "ieee754.ddl";
  include "integer.ddl";

  type uint32 is integer { size: 32, signed: false };
  type uint16 is integer { size: 16, signed: false };
  type uint8 is integer { size: 8, signed: false };

  type ntp_date is ieee754_float {
    bias: -2208988800.0,
    representation: uint32,
  };

  type ntp_timestamp is struct {
    seconds: ntp_date { align: 4 },
    fraction: uint32 { align: 4 },
  };

  type poll is uint8;
  type precision is uint8;
  type delay is uint16;
  type dispersion is uint16;

  type ntp_packet is struct {
    leap: uint8 { bits: 2 },
    version: uint8 { bits: 3 },
    mode: uint8 { bits: 3 },
    poll: poll,
    precision: precision,
    delay: delay,
    dispersion: dispersion,
    identifier: uint32,
    reference_timestamp: ntp_timestamp,
    originate_timestamp: ntp_timestamp,
    receive_timestamp: ntp_timestamp,
    transmit_timestamp: ntp_timestamp,
  };

  type network_time_protocol_version_4 is ntp_packet { align: 1 };
}