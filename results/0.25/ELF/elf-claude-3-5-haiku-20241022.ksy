meta:
  id: elf
  title: ELF (Executable and Linkable Format) File
  file-extension: 
    - elf
    - o
    - so
  endian:
    switch-on: e_ident[5]
    cases:
      1: le
      2: be

seq:
  - id: e_ident
    type: ident_struct
    size: 16
  - id: header
    type: elf_header

types:
  ident_struct:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46]
      - id: file_class
        type: u1
        enum: elf_class
      - id: data_encoding
        type: u1
        enum: data_encoding
      - id: elf_version
        type: u1
        enum: version_enum
      - id: os_abi
        type: u1
        enum: os_abi
      - id: abi_version
        type: u1
      - id: padding
        size: 7

  elf_header:
    seq:
      - id: e_type
        type: u2
        enum: obj_file_type
      - id: e_machine
        type: u2
        enum: machine_type
      - id: e_version
        type: u4
        enum: version_enum
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

enums:
  elf_class:
    0: none
    1: class32
    2: class64

  data_encoding:
    0: none
    1: little_endian
    2: big_endian

  version_enum:
    0: none
    1: current

  os_abi:
    0: sysv
    1: hpux
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
    64: arm_aeabi
    97: arm

  obj_file_type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core
    0xfe00: loos
    0xfeff: hios
    0xff00: loproc
    0xffff: hiproc

  machine_type:
    0: none
    1: m32
    2: sparc
    3: x86
    4: m68k
    5: m88k
    6: mips
    7: ppc
    8: s390
    9: arm
    10: superh
    11: ia64
    12: mips_x
    13: coldfire
    14: m68hc12
    15: mips_rs3_le
    16: riscv
    17: arm64
    18: avr
    19: h8_300
    20: h8_300h
    21: h8s
    22: h8_500
    23: ia32
    24: x86_64
    25: pdp11
    26: aarch64
    27: avr32
    28: alpha
    29: cris
    30: frv
    31: in3d
    32: msp430
    33: openrisc
    34: arc
    35: xtensa
    36: microblaze
    37: nios2
    38: blackfin
    39: mips_pic
    40: mips_elf
    41: nxp_56800
    42: msp430_14
    43: vidcore
    44: tms320c6000
    45: mcst_elbrus
    46: arm_64
    47: amd_gpu
    48: riscv_old
    50: bpf
    51: csky
    52: loongarch