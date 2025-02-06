meta:
  id: icmp
  endian: be
seq:
  - id: type_field
    type: u1
  - id: code_field
    type: u1
  - id: checksum_field
    type: u2
instances:
  body:
    type: switch-on type_field
    cases:
      0: 
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
          - id: data
            type: u8
      3: 
        seq:
          - id: unused_1
            type: u2
          - id: mtu
            type: u2
          - id: code_switch
            type: switch-on code_field
            cases:
              '0': 
                - id: unused_2
                  type: u4
              '1': 
                - id: unused_3
                  type: u4
              '2': 
                - id: unused_4
                  type: u4
              '3': 
                - id: unused_5
                  type: u4
              '4': 
                - id: unused_6
                  type: u4
              '5': 
                - id: unused_7
                  type: u4
              '6-15': 
                - id: unused_8
                  type: u4
          - id: data
            type: u8
      4: 
        seq:
          - id: unused_9
            type: u2
          - id: unused_10
            type: u2
          - id: data
            type: u8
      5: 
        seq:
          - id: unused_11
            type: u2
          - id: gateway_internet_address
            type: u4
          - id: data
            type: u8
      8: 
        seq:
          - id: identifier
            type: u2
          - id: sequence_number
            type: u2
          - id: data
            type: u8
      11: 
        seq:
          - id: unused_12
            type: u2
          - id: code_switch_2
            type: switch-on code_field
            cases:
              '0': 
                - id: unused_13
                  type: u4
              '1': 
                - id: unused_14
                  type: u4
              '2-15': 
                - id: unused_15
                  type: u4
          - id: data
            type: u8
      12: 
        seq:
          - id: pointer
            type: u1
          - id: unused_16
            type: u3
          - id: code_switch_3
            type: switch-on code_field
            cases:
              '0': 
                - id: unused_17
                  type: u4
              '1': 
                - id: unused_18
                  type: u4
              '2': 
                - id: unused_19
                  type: u4
              '3-15': 
                - id: unused_20
                  type: u4
          - id: data
            type: u8
      13: 
        seq:
          - id: identifier
            type: u4
          - id: sequence_number
            type: u4
          - id: originate_timestamp
            type: u4
          - id: receive_timestamp
            type: u4
          - id: transmit_timestamp
            type: u4
          - id: data
            type: u8
      14: 
        seq:
          - id: identifier
            type: u4
          - id: sequence_number
            type: u4
          - id: originate_timestamp
            type: u4
          - id: receive_timestamp
            type: u4
          - id: transmit_timestamp
            type: u4
          - id: data
            type: u8
      15: 
        seq:
          - id: unused_21
            type: u2
          - id: unused_22
            type: u2
          - id: unused_23
            type: u4
          - id: unused_24
            type: u4
          - id: data
            type: u8
      16: 
        seq:
          - id: unused_25
            type: u2
          - id: unused_26
            type: u2
          - id: unused_27
            type: u4
          - id: unused_28
            type: u4
          - id: data
            type: u8