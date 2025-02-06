meta:
  id: arp
  endian: be
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
        "1": 
          type: bytes
          size: 6
        "6": 
          type: bytes
          size: 8
        "default": 
          type: bytes
          size: hlen
  - id: spa
    type:
      switch-on: ptype
      cases:
        "2048": 
          type: bytes
          size: 4
        "34525": 
          type: bytes
          size: 2
        "default": 
          type: bytes
          size: plen
  - id: tha
    type:
      switch-on: htype
      cases:
        "1": 
          type: bytes
          size: 6
        "6": 
          type: bytes
          size: 8
        "default": 
          type: bytes
          size: hlen
  - id: tpa
    type:
      switch-on: ptype
      cases:
        "2048": 
          type: bytes
          size: 4
        "34525": 
          type: bytes
          size: 2
        "default": 
          type: bytes
          size: plen