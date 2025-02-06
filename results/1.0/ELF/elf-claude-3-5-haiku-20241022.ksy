meta:
  id: elf
  title: Executable and Linkable Format
  endian: 
    switch-on: header.e_ident_struct.data_encoding
    cases:
      1: le
      2: be

seq:
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident_struct
        type: e_ident_struct
      - id: e_type
        type: u2
        enum: elf_type
      - id: e_machine
        type: u2
        enum: elf_machine
      - id: e_version
        type: u4
        enum: elf_version
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

  e_ident_struct:
    seq:
      - id: magic
        contents: [0x7f, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_class
      - id: data_encoding
        type: u1
        enum: elf_data_encoding
      - id: version
        type: u1
        enum: elf_version
      - id: os_abi
        type: u1
        enum: elf_os_abi
      - id: abi_version
        type: u1
      - id: padding
        size: 7

enums:
  elf_type:
    0: type_none
    1: type_relocatable
    2: type_executable
    3: type_shared
    4: type_core

  elf_machine:
    0: machine_none
    3: machine_x86
    62: machine_x86_64
    40: machine_arm

  elf_version:
    0: version_none
    1: version_current

  elf_class:
    0: class_none
    1: class_32bit
    2: class_64bit

  elf_data_encoding:
    0: encoding_none
    1: encoding_little_endian
    2: encoding_big_endian

  elf_os_abi:
    0: abi_system_v
    3: abi_linux
    6: abi_solaris
    7: abi_aix
    8: abi_irix
    9: abi_freebsd
    12: abi_openbsd
    97: abi_arm