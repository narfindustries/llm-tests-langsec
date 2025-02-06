meta:
  id: elf
  endian: le

seq:
  - id: e_ident
    type: e_ident
  - id: e_type
    type: u2
    enum: e_type
  - id: e_machine
    type: u2
    enum: e_machine
  - id: e_version
    type: u4
    enum: e_version
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
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: e_phnum
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: e_shnum

types:
  e_ident:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: ei_class
        type: u1
        enum: ei_class
      - id: ei_data
        type: u1
        enum: ei_data
      - id: ei_version
        type: u1
        enum: ei_version
      - id: ei_osabi
        type: u1
        enum: ei_osabi
      - id: ei_abiversion
        type: u1
      - id: ei_pad
        size: 7

  program_header:
    seq:
      - id: p_type
        type: u4
        enum: p_type
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
      - id: p_flags
        type: u4
        enum: p_flags
      - id: p_align
        type: u8

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
        enum: sh_type
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
  ei_class:
    0: ELFCLASSNONE
    1: ELFCLASS32
    2: ELFCLASS64

  ei_data:
    0: ELFDATANONE
    1: ELFDATA2LSB
    2: ELFDATA2MSB

  ei_version:
    0: EV_NONE
    1: EV_CURRENT

  ei_osabi:
    0: ELFOSABI_NONE
    1: ELFOSABI_HPUX
    2: ELFOSABI_NETBSD
    3: ELFOSABI_LINUX
    6: ELFOSABI_SOLARIS
    7: ELFOSABI_AIX
    8: ELFOSABI_IRIX
    9: ELFOSABI_FREEBSD
    10: ELFOSABI_TRU64
    11: ELFOSABI_MODESTO
    12: ELFOSABI_OPENBSD
    64: ELFOSABI_ARM_AEABI
    97: ELFOSABI_ARM
    255: ELFOSABI_STANDALONE

  e_type:
    0: ET_NONE
    1: ET_REL
    2: ET_EXEC
    3: ET_DYN
    4: ET_CORE
    0xFE00: ET_LOOS
    0xFEFF: ET_HIOS
    0xFF00: ET_LOPROC
    0xFFFF: ET_HIPROC

  e_machine:
    0: EM_NONE
    1: EM_M32
    2: EM_SPARC
    3: EM_386
    4: EM_68K
    5: EM_88K
    7: EM_860
    8: EM_MIPS
    20: EM_PPC
    21: EM_PPC64
    40: EM_ARM
    62: EM_X86_64
    183: EM_AARCH64
    243: EM_RISCV

  e_version:
    0: EV_NONE
    1: EV_CURRENT

  p_type:
    0: PT_NULL
    1: PT_LOAD
    2: PT_DYNAMIC
    3: PT_INTERP
    4: PT_NOTE
    5: PT_SHLIB
    6: PT_PHDR
    7: PT_TLS
    0x60000000: PT_LOOS
    0x6FFFFFFF: PT_HIOS
    0x70000000: PT_LOPROC
    0x7FFFFFFF: PT_HIPROC

  p_flags:
    0x1: PF_X
    0x2: PF_W
    0x4: PF_R
    0x0FF00000: PF_MASKOS
    0xF0000000: PF_MASKPROC

  sh_type:
    0: SHT_NULL
    1: SHT_PROGBITS
    2: SHT_SYMTAB
    3: SHT_STRTAB
    4: SHT_RELA
    5: SHT_HASH
    6: SHT_DYNAMIC
    7: SHT_NOTE
    8: SHT_NOBITS
    9: SHT_REL
    10: SHT_SHLIB
    11: SHT_DYNSYM
    14: SHT_INIT_ARRAY
    15: SHT_FINI_ARRAY
    16: SHT_PREINIT_ARRAY
    0x60000000: SHT_LOOS
    0x6FFFFFFF: SHT_HIOS
    0x70000000: SHT_LOPROC
    0x7FFFFFFF: SHT_HIPROC