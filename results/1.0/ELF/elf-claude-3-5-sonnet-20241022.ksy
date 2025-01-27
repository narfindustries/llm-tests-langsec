meta:
  id: elf
  file-extension: elf
  endian: le
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
  - id: pad
    size: 7
  - id: file_type
    type: u2
    enum: obj_type
  - id: machine
    type: u2
    enum: machine
  - id: e_version
    type: u4
  - id: entry_point
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: program_header_offset
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: section_header_offset
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: flags
    type: u4
  - id: header_size
    type: u2
  - id: program_header_entry_size
    type: u2
  - id: qty_program_header
    type: u2
  - id: section_header_entry_size
    type: u2
  - id: qty_section_header
    type: u2
  - id: section_names_idx
    type: u2
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
    4: hurd
    6: solaris
    7: aix
    8: irix
    9: freebsd
    0xa: tru64
    0xb: modesto
    0xc: openbsd
    0xd: openvms
    0xe: nsk
    0xf: aros
    0x10: fenixos
    0x11: cloudabi
    0x12: openvos
  obj_type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core
  machine:
    0: none
    1: m32
    2: sparc
    3: x86
    4: m68k
    5: m88k
    6: i860
    7: mips
    8: s370
    9: mips_rs3_le
    0xa: sparc64_old
    0xb: i960
    0xc: powerpc
    0xd: powerpc64
    0xe: s390
    0xf: spu
    0x13: arm
    0x14: sh
    0x15: sparcv9
    0x16: h8_300
    0x17: ia64
    0x18: sh64
    0x19: mips64
    0x1a: s390x
    0x28: aarch64
    0x2a: riscv
    0x3e: x86_64
    0xb7: arm64
instances:
  program_headers:
    pos: program_header_offset
    type: program_header
    repeat: expr
    repeat-expr: qty_program_header
  section_headers:
    pos: section_header_offset
    type: section_header
    repeat: expr
    repeat-expr: qty_section_header
types:
  program_header:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: flags_64
        type: u4
        if: _root.bits == bits::b64
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: vaddr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: paddr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: filesz
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: memsz
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: flags_32
        type: u4
        if: _root.bits == bits::b32
      - id: align
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
    enums:
      ph_type:
        0: null_type
        1: load
        2: dynamic
        3: interp
        4: note
        5: shlib
        6: phdr
        7: tls
        0x60000000: loos
        0x6fffffff: hios
        0x70000000: loproc
        0x7fffffff: hiproc
  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: sh_type
      - id: flags
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: addr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: size
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: link
        type: u4
      - id: info
        type: u4
      - id: align
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: entry_size
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
    instances:
      name:
        io: _root._io
        pos: _root.section_headers[_root.section_names_idx].offset + name_offset
        type: strz
        encoding: ASCII
      body:
        io: _root._io
        pos: offset
        size: size
    enums:
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
        14: init_array
        15: fini_array
        16: preinit_array
        17: group
        18: symtab_shndx
        0x60000000: loos
        0x6fffffff: hios
        0x70000000: loproc
        0x7fffffff: hiproc