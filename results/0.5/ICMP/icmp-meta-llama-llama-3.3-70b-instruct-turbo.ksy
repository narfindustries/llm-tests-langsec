meta:
  id: icmp
  endian: be
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: identifier
    type: u2
  - id: sequence_number
    type: u2
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        "0": echo_reply
        "3": destination_unreachable
        "4": source_quench
        "5": 
          type: 
            switch-on: u1
            cases:
              "0": redirect_0
              "1": redirect_1
              "2": redirect_2
              "3": redirect_3
        "8": echo_request
        "11": time_exceeded
        "12": parameter_problem
        "13": timestamp_request
        "14": timestamp_reply
        "15": information_request
        "16": information_reply
types:
  echo_reply:
    seq:
      - id: data
        type: bytes
  destination_unreachable:
    seq:
      - id: unused
        type: u1
      - id: next_hop_mtu
        type: u2
      - id: ip_header
        type: ip_header
  source_quench:
    seq:
      - id: unused
        type: u1
      - id: ip_header
        type: ip_header
  redirect_0:
    seq:
      - id: gateway_address
        type: u3
      - id: ip_header
        type: ip_header
  redirect_1:
    seq:
      - id: gateway_address
        type: u3
      - id: ip_header
        type: ip_header
  redirect_2:
    seq:
      - id: gateway_address
        type: u3
      - id: ip_header
        type: ip_header
  redirect_3:
    seq:
      - id: gateway_address
        type: u3
      - id: ip_header
        type: ip_header
  echo_request:
    seq:
      - id: data
        type: bytes
  time_exceeded:
    seq:
      - id: unused
        type: u1
      - id: ip_header
        type: ip_header
  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: ip_header
        type: ip_header
  timestamp_request:
    seq:
      - id: originate_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4
  timestamp_reply:
    seq:
      - id: originate_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4
  information_request:
    seq:
      - id: unused
        type: u2
  information_reply:
    seq:
      - id: id
        type: u2
      - id: sequence_number
        type: u2
  ip_header:
    seq:
      - id: version_and_header_length
        type: u1
      - id: type_of_service
        type: u1
      - id: total_length
        type: u2
      - id: identification
        type: u2
      - id: flags_and_fragment_offset
        type: u2
      - id: time_to_live
        type: u1
      - id: protocol
        type: u1
      - id: header_checksum
        type: u2
      - id: source_address
        type: u4
      - id: destination_address
        type: u4
      - id: options
        type: bytes