meta:
  id: icmp
  title: Internet Control Message Protocol
  file-format: network-protocol
seq:
  - id: type
    size: 1
  - id: code
    size: 1
  - id: checksum
    size: 2
  - id: body
    process: x => x.type != 0
    type: switch-on type
    cases:
      0:
        type: echo-reply
      8:
        type: echo-request
      3:
        type: destination-unreachable
      11:
        type: time-exceeded
      12:
        type: parameter-problem
types:
  echo-reply:
    seq:
      - id: id
        size: 2
      - id: seq
        size: 2
  echo-request:
    seq:
      - id: id
        size: 2
      - id: seq
        size: 2
  destination-unreachable:
    seq:
      - id: unused
        size: 2
      - id: next-hop-mtu
        size: 2
  time-exceeded:
    seq:
      - id: unused
        size: 2
      - id: unused-2
        size: 2
  parameter-problem:
    seq:
      - id: pointer
        size: 1
      - id: unused
        size: 3