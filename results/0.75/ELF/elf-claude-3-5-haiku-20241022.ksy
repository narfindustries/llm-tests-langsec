meta:
  id: elf
  file-extension: [bin, elf]
  endian:
    switch-on: header.e_ident.class
    cases:
      1: le
      2: be

seq:
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident
        type: ident_struct
      - id: e_type
        type: u2
        enum: obj_type
      - id: e_machine
        type: u2
        enum: arch_type
      - id: e_version
        type: u4
      - id: e_entry
        type: u8
        if: (e_ident.class == elf_class::class_64)
      - id: e_entry_32
        type: u4
        if: (e_ident.class == elf_class::class_32)
      - id: e_phoff
        type: u8
        if: (e_ident.class == elf_class::class_64)
      - id: e_phoff_32
        type: u4
        if: (e_ident.class == elf_class::class_32)
      - id: e_shoff
        type: u8
        if: (e_ident.class == elf_class::class_64)
      - id: e_shoff_32
        type: u4
        if: (e_ident.class == elf_class::class_32)
      - id: e_flags
        type: u4
      - id: e_ehsize
        type: u2
      - id: e_phentsize
        type: u2
      - id: e_phnum
        type: u2
      - id: e_shentsize
        type: u2
      - id: e_shnum
        type: u2
      - id: e_shstrndx
        type: u2

  ident_struct:
    seq:
      - id: magic
        contents: [0x7f, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_class
      - id: data_encoding
        type: u1
        enum: data_encoding_type
      - id: version
        type: u1
      - id: os_abi
        type: u1
        enum: os_abi_type
      - id: abi_version
        type: u1
      - id: padding
        size: 7

enums:
  elf_class:
    1: class_32
    2: class_64

  data_encoding_type:
    1: little_endian
    2: big_endian

  os_abi_type:
    0x00: system_v
    0x01: hp_ux
    0x02: netbsd
    0x03: linux
    0x06: solaris
    0x09: freebsd
    0x0C: openbsd
    0xFF: standalone

  obj_type:
    0x00: et_none
    0x01: et_rel
    0x02: et_exec
    0x03: et_dyn
    0x04: et_core

  arch_type:
    0x02: sparc
    0x03: x86
    0x08: mips
    0x14: powerpc
    0x28: arm
    0x32: ia64
    0x3E: x86_64
    0xB7: aarch64