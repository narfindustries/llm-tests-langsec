meta:
  id: elf
  title: Executable and Linkable Format
  application: executable format
  file-extension:
    - elf
    - o
    - so
    - ko
  license: CC0-1.0
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
  - id: type
    type: u2
    enum: obj_type
  - id: machine
    type: u2
    enum: machine
  - id: version
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
  - id: program_header_num_entries
    type: u2
  - id: section_header_entry_size
    type: u2
  - id: section_header_num_entries
    type: u2
  - id: section_names_idx
    type: u2
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: program_header_num_entries
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: section_header_num_entries

types:
  program_header:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: flags
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
    3: i386
    4: m68k
    5: m88k
    7: i860
    8: mips
    62: x86_64

  ph_type:
    0x00000000: pt_null
    0x00000001: pt_load
    0x00000002: pt_dynamic
    0x00000003: pt_interp
    0x00000004: pt_note
    0x00000005: pt_shlib
    0x00000006: pt_phdr
    0x00000007: pt_tls
    0x60000000: pt_loos
    0x6fffffff: pt_hios
    0x70000000: pt_loproc
    0x7fffffff: pt_hiproc

  sh_type:
    0x00000000: null_type
    0x00000001: progbits
    0x00000002: symtab
    0x00000003: strtab
    0x00000004: rela
    0x00000005: hash
    0x00000006: dynamic
    0x00000007: note
    0x00000008: nobits
    0x00000009: rel
    0x0000000a: shlib
    0x0000000b: dynsym
    0x0000000e: init_array
    0x0000000f: fini_array
    0x00000010: preinit_array
    0x00000011: group
    0x00000012: symtab_shndx
    0x60000000: loos
    0x6fffffff: hios
    0x70000000: loproc
    0x7fffffff: hiproc