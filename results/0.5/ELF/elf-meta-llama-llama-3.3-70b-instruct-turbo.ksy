meta:
  id: elf
  endian: le
seq:
  - id: e_ident
    type: e_ident
  - id: e_type
    type: uint16
  - id: e_machine
    type: uint16
  - id: e_version
    type: uint32
  - id: e_entry
    type:
      switch-on: e_ident.ei_class
      cases:
        1: uint32
        2: uint64
  - id: e_phoff
    type:
      switch-on: e_ident.ei_class
      cases:
        1: uint32
        2: uint64
  - id: e_shoff
    type:
      switch-on: e_ident.ei_class
      cases:
        1: uint32
        2: uint64
  - id: e_flags
    type: uint32
  - id: e_ehsize
    type: uint16
  - id: e_phentsize
    type: uint16
  - id: e_phnum
    type: uint16
  - id: e_shentsize
    type: uint16
  - id: e_shnum
    type: uint16
  - id: e_shstrndx
    type: uint16
  - id: program_headers
    type: program_header
    repeat: expr
      value: e_phnum
  - id: section_headers
    type: section_header
    repeat: expr
      value: e_shnum
types:
  e_ident:
    seq:
      - id: ei_mag
        type: str
        size: 4
      - id: ei_class
        type: uint8
      - id: ei_data
        type: uint8
      - id: ei_version
        type: uint8
      - id: ei_osabi
        type: uint8
      - id: ei_abiversion
        type: uint8
      - id: ei_pad
        type: str
        size: 7
  program_header:
    seq:
      - id: p_type
        type: uint32
      - id: p_offset
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: p_vaddr
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: p_paddr
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: p_filesz
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: p_memsz
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: p_flags
        type: uint32
      - id: p_align
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
  section_header:
    seq:
      - id: sh_name
        type: uint32
      - id: sh_type
        type: uint32
      - id: sh_flags
        type: uint32
      - id: sh_addr
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: sh_offset
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: sh_size
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: sh_link
        type: uint32
      - id: sh_info
        type: uint32
      - id: sh_addralign
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: sh_entsize
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
  symbol:
    seq:
      - id: st_name
        type: uint32
      - id: st_value
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: st_size
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: st_info
        type: uint8
      - id: st_other
        type: uint8
      - id: st_shndx
        type: uint16
  relocation:
    seq:
      - id: r_offset
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64
      - id: r_info
        type: uint32
      - id: r_addend
        type:
          switch-on: _root.e_ident.ei_class
          cases:
            1: uint32
            2: uint64