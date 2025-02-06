meta:
  id: elf
  title: ELF (Executable and Linkable Format) File
  file-extension: 
    - elf
    - so
    - o
  endian:
    switch-on: header.e_ident.data_encoding
    cases:
      'data_encoding::little_endian': le
      'data_encoding::big_endian': be

enums:
  file_class:
    0x00: none
    0x01: class32
    0x02: class64

  data_encoding:
    0x00: none
    0x01: little_endian
    0x02: big_endian

  os_abi:
    0x00: system_v
    0x01: hp_ux
    0x02: netbsd
    0x03: linux
    0x06: solaris
    0x09: freebsd
    0x0C: arm

  object_file_type:
    0x00: none
    0x01: relocatable
    0x02: executable
    0x03: shared_object
    0x04: core

  machine_architecture:
    0x02: sparc
    0x03: x86
    0x08: mips
    0x14: powerpc
    0x28: arm
    0x3E: x86_64

types:
  ident_struct:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: file_class
        type: u1
        enum: file_class
      - id: data_encoding
        type: u1
        enum: data_encoding
      - id: version
        type: u1
      - id: os_abi
        type: u1
        enum: os_abi
      - id: abi_version
        type: u1
      - id: padding
        size: 7

  elf_header:
    seq:
      - id: e_ident
        type: ident_struct
      - id: e_type
        type: u2
        enum: object_file_type
      - id: e_machine
        type: u2
        enum: machine_architecture
      - id: e_version
        type: u4
      - id: e_entry
        type: u8
      - id: e_phoff
        type: u8
      - id: e_shoff
        type: u8
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

  program_header:
    seq:
      - id: p_type
        type: u4
      - id: p_flags
        type: u4
      - id: p_offset
        type: u8
      - id: p_vaddr
        type: u8
      - id: p_paddr
        type: u8
      - id: p_filesz
        type: u8
      - id: p_memsz
        type: u8
      - id: p_align
        type: u8

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
      - id: sh_flags
        type: u8
      - id: sh_addr
        type: u8
      - id: sh_offset
        type: u8
      - id: sh_size
        type: u8
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u8
      - id: sh_entsize
        type: u8

seq:
  - id: header
    type: elf_header
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: header.e_phnum
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: header.e_shnum