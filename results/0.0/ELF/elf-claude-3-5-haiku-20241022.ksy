meta:
  id: elf
  title: ELF (Executable and Linkable Format) file format
  file-extension: 
    - elf
    - o
    - so
  license: MIT
  endian: be

seq:
  - id: header
    type: elf_header
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: header.program_header_num
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: header.section_header_num

types:
  elf_header:
    seq:
      - id: magic
        contents: [0x7F, 0x45, 0x4C, 0x46]
      - id: class
        type: u1
        enum: elf_class
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
      - id: type
        type: u2
        enum: elf_type
      - id: machine
        type: u2
        enum: elf_machine
      - id: version_2
        type: u4
      - id: entry_point_addr
        type: u8
      - id: program_header_offset
        type: u8
      - id: section_header_offset
        type: u8
      - id: flags
        type: u4
      - id: header_size
        type: u2
      - id: program_header_size
        type: u2
      - id: program_header_num
        type: u2
      - id: section_header_size
        type: u2
      - id: section_header_num
        type: u2
      - id: section_header_str_index
        type: u2

  program_header:
    seq:
      - id: type
        type: u4
        enum: program_header_type
      - id: flags
        type: u4
      - id: offset
        type: u8
      - id: vaddr
        type: u8
      - id: paddr
        type: u8
      - id: file_size
        type: u8
      - id: mem_size
        type: u8
      - id: align
        type: u8

  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: section_header_type
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
      - id: addr_align
        type: u8
      - id: entry_size
        type: u8

enums:
  elf_class:
    1: class_32
    2: class_64

  data_encoding:
    1: little_endian
    2: big_endian

  os_abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    6: solaris
    7: aix
    8: irix
    9: freebsd
    0x0C: openbsd

  elf_type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core

  elf_machine:
    0x02: sparc
    0x03: x86
    0x08: mips
    0x14: powerpc
    0x28: arm
    0x2A: superh
    0x32: ia_64
    0x3E: x86_64
    0xB7: aarch64

  program_header_type:
    0: null
    1: load
    2: dynamic
    3: interp
    4: note
    5: shlib
    6: phdr
    7: tls

  section_header_type:
    0: null
    1: progbits
    2: symtab
    3: strtab
    4: rela
    5: hash
    6: dynamic
    7: note
    8: nobits
    9: rel
    11: dynsym