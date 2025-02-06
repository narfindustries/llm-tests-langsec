meta:
  id: elf
  file-extension: elf
  endian: le
  bit-endian: le

seq:
  - id: magic
    contents: [0x7f, "ELF"]
  - id: ei_class
    type: u1
    enum: class
  - id: ei_data
    type: u1
    enum: endian
  - id: ei_version
    type: u1
  - id: ei_osabi
    type: u1
    enum: os_abi
  - id: ei_abiversion
    type: u1
  - id: ei_pad
    size: 7
  - id: e_type
    type: u2
    enum: e_type
  - id: e_machine
    type: u2
    enum: machine
  - id: e_version
    type: u4
  - id: e_entry
    type: u4
    if: ei_class == class::class_32
  - id: e_entry_64
    type: u8
    if: ei_class == class::class_64
  - id: e_phoff
    type: u4
    if: ei_class == class::class_32
  - id: e_phoff_64
    type: u8
    if: ei_class == class::class_64
  - id: e_shoff
    type: u4
    if: ei_class == class::class_32
  - id: e_shoff_64
    type: u8
    if: ei_class == class::class_64
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
  program_header:
    seq:
      - id: p_type
        type: u4
        enum: ph_type
      - id: p_flags
        type: u4
        if: _root.ei_class == class::class_64
      - id: p_offset
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_offset_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: p_vaddr
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_vaddr_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: p_paddr
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_paddr_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: p_filesz
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_filesz_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: p_memsz
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_memsz_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: p_flags_32
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_align
        type: u4
        if: _root.ei_class == class::class_32
      - id: p_align_64
        type: u8
        if: _root.ei_class == class::class_64

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
        enum: sh_type
      - id: sh_flags
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_flags_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: sh_addr
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_addr_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: sh_offset
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_offset_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: sh_size
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_size_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_addralign_64
        type: u8
        if: _root.ei_class == class::class_64
      - id: sh_entsize
        type: u4
        if: _root.ei_class == class::class_32
      - id: sh_entsize_64
        type: u8
        if: _root.ei_class == class::class_64

enums:
  class:
    1: class_32
    2: class_64

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

  e_type:
    0: et_none
    1: et_rel
    2: et_exec
    3: et_dyn
    4: et_core
    0xfe00: et_loos
    0xfeff: et_hios
    0xff00: et_loproc
    0xffff: et_hiproc

  machine:
    0: em_none
    1: em_m32
    2: em_sparc
    3: em_386
    4: em_68k
    5: em_88k
    7: em_860
    8: em_mips
    62: em_x86_64

  ph_type:
    0: pt_null
    1: pt_load
    2: pt_dynamic
    3: pt_interp
    4: pt_note
    5: pt_shlib
    6: pt_phdr
    7: pt_tls
    0x60000000: pt_loos
    0x6fffffff: pt_hios
    0x70000000: pt_loproc
    0x7fffffff: pt_hiproc

  sh_type:
    0: sh_null
    1: sh_progbits
    2: sh_symtab
    3: sh_strtab
    4: sh_rela
    5: sh_hash
    6: sh_dynamic
    7: sh_note
    8: sh_nobits
    9: sh_rel
    10: sh_shlib
    11: sh_dynsym