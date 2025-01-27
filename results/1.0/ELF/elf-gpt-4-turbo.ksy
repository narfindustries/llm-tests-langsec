meta:
  id: elf
  file-extension: elf
  endian: le
  license: MIT
seq:
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46]
  - id: class
    type: u1
    enum: elf_class
  - id: data
    type: u1
    enum: elf_data_encoding
  - id: version
    contents: [0x01]
  - id: os_abi
    type: u1
    enum: elf_os_abi
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: header
    type:
      switch-on: class
      cases:
        elf_class::elf32: elf_header_32
        elf_class::elf64: elf_header_64
types:
  elf_header_32:
    seq:
      - id: type
        type: u2
        enum: e_type
      - id: machine
        type: u2
        enum: machine
      - id: version
        type: u4
      - id: entry_point
        type: u4
      - id: program_header_offset
        type: u4
      - id: section_header_offset
        type: u4
      - id: flags
        type: u4
      - id: header_size
        type: u2
      - id: program_header_entry_size
        type: u2
      - id: program_header_num_entries
        type: u2
      - id: section_header_entry_size
        type: u2
      - id: section_header_num_entries
        type: u2
      - id: section_names_idx
        type: u2
  elf_header_64:
    seq:
      - id: type
        type: u2
        enum: e_type
      - id: machine
        type: u2
        enum: machine
      - id: version
        type: u4
      - id: entry_point
        type: u8
      - id: program_header_offset
        type: u8
      - id: section_header_offset
        type: u8
      - id: flags
        type: u4
      - id: header_size
        type: u2
      - id: program_header_entry_size
        type: u2
      - id: program_header_num_entries
        type: u2
      - id: section_header_entry_size
        type: u2
      - id: section_header_num_entries
        type: u2
      - id: section_names_idx
        type: u2
enums:
  elf_class:
    0x01: elf32
    0x02: elf64
  elf_data_encoding:
    0x01: le
    0x02: be
  elf_os_abi:
    0x00: system_v
    0x01: hp_ux
    # Other ABI enum options skipped for brevity
  e_type:
    0x00: none
    0x01: rel
    0x02: exec
    # Other type enum options skipped for brevity
  machine:
    0x0000: no_machine
    0x003E: x86_64
    # Other machine enum options skipped for brevity