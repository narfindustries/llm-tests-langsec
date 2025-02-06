meta:
  id: elf
  file-extension: elf
  endian: le
  bit-endian: le
seq:
  - id: magic
    contents: [0x7f, "ELF"]
  - id: bits
    type: u1
    enum: bits
  - id: endian
    type: u1
    enum: endian
  - id: ei_version
    type: u1
  - id: abi
    type: u1
    enum: os_abi
  - id: abi_version
    type: u1
  - id: padding
    size: 7
  - id: type
    type: u2
    enum: object_type
  - id: machine
    type: u2
    enum: machine
  - id: version
    type: u4
  - id: entry_point
    type: u4
    if: bits == bits::b32
  - id: entry_point_64
    type: u8
    if: bits == bits::b64
  - id: program_header_offset
    type: u4
    if: bits == bits::b32
  - id: program_header_offset_64
    type: u8
    if: bits == bits::b64
  - id: section_header_offset
    type: u4
    if: bits == bits::b32
  - id: section_header_offset_64
    type: u8
    if: bits == bits::b64
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
instances:
  program_headers:
    pos: program_header_offset
    type: program_header
    repeat: expr
    repeat-expr: program_header_num_entries
    if: bits == bits::b32
  program_headers_64:
    pos: program_header_offset_64
    type: program_header_64
    repeat: expr
    repeat-expr: program_header_num_entries
    if: bits == bits::b64
  section_headers:
    pos: section_header_offset
    type: section_header
    repeat: expr
    repeat-expr: section_header_num_entries
    if: bits == bits::b32
  section_headers_64:
    pos: section_header_offset_64
    type: section_header_64
    repeat: expr
    repeat-expr: section_header_num_entries
    if: bits == bits::b64
types:
  program_header:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: offset
        type: u4
      - id: vaddr
        type: u4
      - id: paddr
        type: u4
      - id: filesz
        type: u4
      - id: memsz
        type: u4
      - id: flags
        type: u4
      - id: align
        type: u4
  program_header_64:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: flags
        type: u4
      - id: offset
        type: u8
      - id: vaddr
        type: u8
      - id: paddr
        type: u8
      - id: filesz
        type: u8
      - id: memsz
        type: u8
      - id: align
        type: u8
  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: sh_type
      - id: flags
        type: u4
      - id: addr
        type: u4
      - id: offset
        type: u4
      - id: size
        type: u4
      - id: link
        type: u4
      - id: info
        type: u4
      - id: align
        type: u4
      - id: entry_size
        type: u4
  section_header_64:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: sh_type
      - id: flags
        type: u8
      - id: addr
        type: u8
      - id: offset
        type: u8
      - id: size
        type: u8
      - id: link
        type: u4
      - id: info
        type: u4
      - id: align
        type: u8
      - id: entry_size
        type: u8
enums:
  bits:
    1: b32
    2: b64
  endian:
    1: le
    2: be
  os_abi:
    0: system_v
    1: hp_ux
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
  object_type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
    0xfe00: loos
    0xfeff: hios
    0xff00: loproc
    0xffff: hiproc
  machine:
    0: none
    1: m32
    2: sparc
    3: i386
    4: m68k
    5: m88k
    7: i860
    8: mips
    62: x86_64
  ph_type:
    0x0: pt_null
    0x1: pt_load
    0x2: pt_dynamic
    0x3: pt_interp
    0x4: pt_note
    0x5: pt_shlib
    0x6: pt_phdr
    0x7: pt_tls
    0x60000000: pt_loos
    0x6fffffff: pt_hios
    0x70000000: pt_loproc
    0x7fffffff: pt_hiproc
  sh_type:
    0: null_type
    1: progbits
    2: symtab
    3: strtab
    4: rela
    5: hash
    6: dynamic
    7: note
    8: nobits
    9: rel
    10: shlib
    11: dynsym