meta:
  id: elf
  title: ELF (Executable and Linkable Format)
  endian: le

seq:
  - id: e_ident
    type: str
    size: 16
    encoding: ascii
    doc: ELF magic number and identification

  - id: e_type
    type: uint16
    enum:
      ET_REL: 1
      ET_EXEC: 2
      ET_DYN: 3
      ET_CORE: 4
    doc: Object file type

  - id: e_machine
    type: uint16
    enum:
      EM_M32: 1
      EM_SPARC: 2
      EM_386: 3
      EM_68K: 4
      EM_88K: 5
      EM_860: 6
      EM_MIPS: 7
      EM_S370: 8
      EM_MIPS_RS3_LE: 9
      EM_PARISC: 10
      EM_VPP500: 11
      EM_SPARC32PLUS: 12
      EM_960: 13
      EM_PPC: 14
      EM_PPC64: 15
      EM_S390: 16
      EM_SPU: 17
      EM_V800: 18
      EM_FR20: 19
      EM_RH32: 20
      EM_RCE: 21
      EM_ARM: 22
      EM_ALPHA: 23
      EM_SH: 24
      EM_SPARCV9: 25
      EM_TRICORE: 26
      EM_ARC: 27
      EM_H8_300: 28
      EM_H8_300H: 29
      EM_H8S: 30
      EM_H8_500: 31
      EM_IA_64: 32
      EM_MIPS_X: 33
      EM_COLDFIRE: 34
      EM_68HC12: 35
      EM_MMA: 36
      EM_PCP: 37
      EM_NCPU: 38
      EM_NDR1: 39
      EM_STEALTH: 40
      EM_ETPU: 41
      EM_ST200: 42
      EM_IP2K: 43
      EM_MAX: 44
      EM_CR: 45
      EM_F2MC16: 46
      EM_M16C: 47
      EM_XSTORMY16: 48
      EM_TX39: 49
      EM_NM32: 50
      EM_MN10300: 51
      EM_MN10200: 52
      EM_PJ: 53
      EM_OPENRISC: 54
      EM_ARC_A5: 55
      EM_XTENSA: 56
      EM_AARCH64: 57
      EM_TILEPRO: 58
      EM_MICROBLAZE: 59
      EM_TILEGX: 60
      EM_C6X: 61
      EM_C67: 62
      EM_AARCH64_BE: 63
      EM_370: 64
      EM_M32R: 65
    doc: Architecture

  - id: e_version
    type: uint32
    doc: Object file version

  - id: e_entry
    type:
      switch-on: e_ident.e_ident[4]
      cases:
        1: uint32
        2: uint64
    doc: Entry point virtual address

  - id: e_phoff
    type:
      switch-on: e_ident.e_ident[4]
      cases:
        1: uint32
        2: uint64
    doc: Program header offset

  - id: e_shoff
    type:
      switch-on: e_ident.e_ident[4]
      cases:
        1: uint32
        2: uint64
    doc: Section header offset

  - id: e_flags
    type: uint32
    doc: Processor-specific flags

  - id: e_ehsize
    type: uint16
    doc: ELF header size

  - id: e_phentsize
    type: uint16
    doc: Program header entry size

  - id: e_phnum
    type: uint16
    doc: Number of program headers

  - id: e_shentsize
    type: uint16
    doc: Section header entry size

  - id: e_shnum
    type: uint16
    doc: Number of section headers

  - id: e_shstrndx
    type: uint16
    doc: Section header string table index

types:
  Elf_Phdr:
    seq:
      - id: p_type
        type: uint32
        enum:
          PT_NULL: 0
          PT_LOAD: 1
          PT_DYNAMIC: 2
          PT_INTERP: 3
          PT_NOTE: 4
          PT_SHLIB: 5
          PT_PHDR: 6
          PT_TLS: 7
        doc: Segment type

      - id: p_offset
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment offset

      - id: p_vaddr
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment virtual address

      - id: p_paddr
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment physical address

      - id: p_filesz
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment size in file

      - id: p_memsz
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment size in memory

      - id: p_flags
        type: uint32
        doc: Segment flags

      - id: p_align
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Segment alignment

  Elf_Shdr:
    seq:
      - id: sh_name
        type: uint32
        doc: Section name offset

      - id: sh_type
        type: uint32
        enum:
          SHT_NULL: 0
          SHT_PROGBITS: 1
          SHT_SYMTAB: 2
          SHT_STRTAB: 3
          SHT_RELA: 4
          SHT_HASH: 5
          SHT_DYNAMIC: 6
          SHT_NOTE: 7
          SHT_NOBITS: 8
          SHT_REL: 9
          SHT_SHLIB: 10
          SHT_DYNSYM: 11
        doc: Section type

      - id: sh_flags
        type: uint32
        doc: Section flags

      - id: sh_addr
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Section virtual address

      - id: sh_offset
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Section offset

      - id: sh_size
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Section size

      - id: sh_link
        type: uint32
        doc: Section link

      - id: sh_info
        type: uint32
        doc: Section information

      - id: sh_addralign
        type:
          switch-on: e_ident.e_ident[4]
          cases:
            1: uint32
            2: uint64
        doc: Section alignment

      - id: sh_entsize
        type: uint32
        doc: Section entry size

instances:
  program_headers:
    type: Elf_Phdr
    repeat: expr
    repeat-expr: e_phnum
    doc: Program headers

  section_headers:
    type: Elf_Shdr
    repeat: expr
    repeat-expr: e_shnum
    doc: Section headers