meta:
  id: elf
  file-extension: elf
  endian: le
  license: MIT
seq:
  - id: magic
    contents: [0x7F, 'E', 'L', 'F']
  - id: class
    type: u1
    enum: elf_class
  - id: data
    type: u1
    enum: elf_data
  - id: version
    type: u1
  - id: os_abi
    type: u1
    enum: elf_os_abi
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
  - id: version_again
    type: u4
  - id: entry_point
    type: u4
  - id: phoff
    type: u4
  - id: shoff
    type: u4
  - id: flags
    type: u4
  - id: ehsize
    type: u2
  - id: phentsize
    type: u2
  - id: phnum
    type: u2
  - id: shentsize
    type: u2
  - id: shnum
    type: u2
  - id: shstrndx
    type: u2
enums:
  elf_class:
    1: elf32
    2: elf64
  elf_data:
    1: little_endian
    2: big_endian
  elf_os_abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    4: gnu_hurd
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: novell_modesto
    12: openbsd
    13: openvms
    14: nonstop_kernel
    15: aros
    16: fenix_os
    17: cloudabi
    18: stratus_openvos
  elf_type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
  elf_machine:
    0: none
    1: m32
    2: sparc
    3: i386
    4: m68k
    5: m88k
    6: i860
    7: mips
    8: powerpc
    9: s390
    10: arm
    11: superh
    12: ia_64
    13: x86_64
    14: aarch64
    15: riscv
types:
  elf32_header:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_class
      - id: data
        type: u1
        enum: elf_data
      - id: version
        type: u1
      - id: os_abi
        type: u1
        enum: elf_os_abi
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
      - id: version_again
        type: u4
      - id: entry_point
        type: u4
      - id: phoff
        type: u4
      - id: shoff
        type: u4
      - id: flags
        type: u4
      - id: ehsize
        type: u2
      - id: phentsize
        type: u2
      - id: phnum
        type: u2
      - id: shentsize
        type: u2
      - id: shnum
        type: u2
      - id: shstrndx
        type: u2
  elf64_header:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_class
      - id: data
        type: u1
        enum: elf_data
      - id: version
        type: u1
      - id: os_abi
        type: u1
        enum: elf_os_abi
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
      - id: version_again
        type: u4
      - id: entry_point
        type: u8
      - id: phoff
        type: u8
      - id: shoff
        type: u8
      - id: flags
        type: u4
      - id: ehsize
        type: u2
      - id: phentsize
        type: u2
      - id: phnum
        type: u2
      - id: shentsize
        type: u2
      - id: shnum
        type: u2
      - id: shstrndx
        type: u2