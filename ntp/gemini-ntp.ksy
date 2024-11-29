type: ntp_packet
endianness: big_endian

seq:
  type: bits
  size: 8

stratum:
  type: bits
  size: 8

poll:
  type: bits
  size: 8

precision:
  type: bits
  size: 8

root_delay:
  type: bits
  size: 32

root_dispersion:
  type: bits
  size: 32

reference_identifier:
  type: strz
  size: 4

reference_timestamp:
  type: ntp_timestamp

originate_timestamp:
  type: ntp_timestamp

receive_timestamp:
  type: ntp_timestamp

transmit_timestamp:
  type: ntp_timestamp

type: ntp_timestamp
endianness: big_endian

seconds:
  type: u4

fraction:
  type: u4