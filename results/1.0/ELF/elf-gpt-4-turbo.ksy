meta:
  id: elf
  endian: le
  title: Executable and Linkable Format (ELF)
seq:
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46]
  - id: bits
    type: u1
  - id: endian
    type: u1
  - id: ei_version
    contents: [0x01]
  - id: os_abi
    type: u1
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: type
    type: u2
  - id: machine
    type: u2
  - id: version
    type: u4
  - id: entry_point
    type: 
      switch-on: bits
      cases:
        1: u4
        2: u8
  - id: program_header_offset
    type: 
      switch-on: bits
      cases:
        1: u4
        2: u8
  - id: section_header_offset
    type: 
      switch-on: bits
      cases:
        1: u4
        2: u8
  - id: flags
    type: u4
  - id: header_size
    type: u2
  - id: program_header_entry_size
    type: u2
  - id: program_header_count
    type: u2
  - id: section_header_entry_size
    type: u2
  - id: section_header_count
    type: u2
  - id: string_table_idx
    type: u2
types:
  program_header:
    seq:
      - id: type
        type: u4
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: vaddr
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: paddr
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: filesz
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: memsz
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: flags32
        type: u4
        if: _root.bits == 1
      - id: flags64
        type: u4
        if: _root.bits == 2
      - id: align
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
      - id: flags
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: addr
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: size
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: link
        type: u4
      - id: info
        type: u4
      - id: addr_align
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
      - id: entry_size
        type:
          switch-on: _root.bits
          cases:
            1: u4
            2: u8
instances:
  program_headers:
    pos: program_header_offset
    type: program_header
    repeat: expr
    repeat-expr: program_header_count
  section_headers:
    pos: section_header_offset
    type: section_header
    repeat: expr
    repeat-expr: section_header_count