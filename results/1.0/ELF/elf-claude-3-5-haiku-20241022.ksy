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
    repeat-expr: header.e_phnum
    if: header.e_phoff != 0
  - id: section_headers
    type: section_header
    repeat: expr 
    repeat-expr: header.e_shnum
    if: header.e_shoff != 0

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
        enum: elf_data_encoding  
      - id: version
        type: u1
      - id: os_abi
        type: u1
        enum: elf_os_abi
      - id: abi_version
        type: u1
      - id: padding
        size: 7
      - id: e_type
        type: u2
        enum: elf_type
      - id: e_machine
        type: u2
        enum: elf_machine
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
        enum: program_type
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
        enum: section_type
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

enums:
  elf_class:
    1: class32
    2: class64

  elf_data_encoding:
    1: little_endian
    2: big_endian

  elf_os_abi:
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
    1: relocatable
    2: executable
    3: shared
    4: core

  elf_machine:
    3: x86
    0x3E: x86_64
    0x28: arm
    0xB7: aarch64

  program_type:
    0: null_type
    1: load
    2: dynamic
    3: interp
    4: note
    5: shlib
    6: phdr
    7: tls

  section_type:
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