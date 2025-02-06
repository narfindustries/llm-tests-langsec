meta:
  id: arp
  endian: be
types:
  mac_addr:
    seq:
      - id: addr
        type: u1
        repeat: 6
  ip_addr:
    seq:
      - id: addr
        type: u1
        repeat: 4
seq:
  - id: htype
    type: u2
  - id: ptype
    type: u2
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: oper
    type: u2
  - id: sha
    type:
      switch-on: htype
      cases:
        1: 
          type: mac_addr
        else: 
          type: 
            seq:
              - id: addr
                type: u1
                repeat: hlen
  - id: spa
    type:
      switch-on: ptype
      cases:
        2048: 
          type: ip_addr
        else: 
          type: 
            seq:
              - id: addr
                type: u1
                repeat: plen
  - id: tha
    type:
      switch-on: htype
      cases:
        1: 
          type: mac_addr
        else: 
          type: 
            seq:
              - id: addr
                type: u1
                repeat: hlen
  - id: tpa
    type:
      switch-on: ptype
      cases:
        2048: 
          type: ip_addr
        else: 
          type: 
            seq:
              - id: addr
                type: u1
                repeat: plen