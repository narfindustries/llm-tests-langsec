meta:
  id: elf
  endian: le
  file-extension: elf
seq:
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46]
  - id: bits
    type: u1
    enum: bits_t
  - id: endian
    type: u1
    enum: endian_t
  - id: ei_version
    contents: [0x01]
  - id: abi
    type: u1
    enum: os_abi
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: header
    type:
      switch-on: bits
      cases:
        bits_t::b32: elf_header_32
        bits_t::b64: elf_header_64
types:
  elf_header_32:
    seq:
      - id: type
        type: u2
        enum: obj_type
      - id: machine
        type: u2
        enum: machine
      - id: version
        type: u4
      - id: entry_point
        type: u4
      - id: prog_hdr_offset
        type: u4
      - id: sect_hdr_offset
        type: u4
      - id: flags
        type: u4
      - id: header_size
        type: u2
      - id: prog_hdr_entry_size
        type: u2
      - id: prog_hdr_entry_count
        type: u2
      - id: sect_hdr_entry_size
        type: u2
      - id: sect_hdr_entry_count
        type: u2
      - id: sect_names_idx
        type: u2
  elf_header_64:
    seq:
      - id: type
        type: u2
        enum: obj_type
      - id: machine
        type: u2
        enum: machine
      - id: version
        type: u4
      - id: entry_point
        type: u8
      - id: prog_hdr_offset
        type: u8
      - id: sect_hdr_offset
        type: u8
      - id: flags
        type: u4
      - id: header_size
        type: u2
      - id: prog_hdr_entry_size
        type: u2
      - id: prog_hdr_entry_count
        type: u2
      - id: sect_hdr_entry_size
        type: u2
      - id: sect_hdr_entry_count
        type: u2
      - id: sect_names_idx
        type: u2
enums:
  bits_t:
    1: b32
    2: b64
  endian_t:
    1: le
    2: be
  os_abi:
    0: system_v
    1: hpux
    2: netbsd
    3: linux
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: modesto
    12: openbsd
    64: arm_aeabi
    97: arm
    255: standalone
  obj_type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
    0xFE00: lo_os
    0xFEFF: hi_os
    0xFF00: lo_proc
    0xFFFF: hi_proc
  machine:
    0: no_machine
    3: x86
    4: mips
    7: intel_mcu
    8: sparc
    9: intel_80860
    10: mips_rs3000_le
    15: parisc
    18: sparc32plus
    19: sparc64
    20: mips_rs3000
    40: arm
    62: x86_64
    183: aarch64
    243: riscv