meta:
  id: icmp
  name: icmp
  file-extension: icmp
  fields:
    - name: type
      type: u1
    - name: code
      type: u1
    - name: checksum
      type: u2
    - name: identifier
      type: u2
    - name: sequence-number
      type: u2
    - name: data
      type: u1
      repeat: until-eof
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: identifier
    type: u2
  - id: sequence-number
    type: u2
  - id: data
    type: u1
    repeat: until-eof