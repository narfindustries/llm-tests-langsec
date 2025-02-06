meta:
  id: elf
  title: ELF (Executable and Linkable Format)
  file-extension: elf
  endian: be

seq:
  - id: e_ident
    type: e_ident
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_type
        type: u2
      - id: e_machine
        type: u2
      - id: e_version
        type: u4
      - id: e_entry
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: e_entry_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: e_phoff
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: e_phoff_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: e_shoff
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: e_shoff_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
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

  e_ident:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46] # "\x7FELF"
      - id: ei_class
        type: u1
        enum: elf_class
      - id: ei_data
        type: u1
        enum: elf_data
      - id: ei_version
        type: u1
      - id: ei_osabi
        type: u1
        enum: elf_osabi
      - id: ei_abiversion
        type: u1
      - id: ei_pad
        size: 7

  program_header:
    seq:
      - id: p_type
        type: u4
        enum: pt_type
      - id: p_offset
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_offset_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: p_vaddr
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_vaddr_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: p_paddr
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_paddr_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: p_filesz
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_filesz_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: p_memsz
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_memsz_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: p_flags
        type: u4
      - id: p_align
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: p_align_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
        enum: sht_type
      - id: sh_flags
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_flags_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: sh_addr
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_addr_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: sh_offset
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_offset_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: sh_size
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_size_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_addralign_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64
      - id: sh_entsize
        type: b4
        if: _root.e_ident.ei_class == elf_class::class_32
      - id: sh_entsize_64
        type: b8
        if: _root.e_ident.ei_class == elf_class::class_64

enums:
  elf_class:
    0: invalid
    1: class_32
    2: class_64

  elf_data:
    0: invalid
    1: lsb
    2: msb

  elf_osabi:
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
    13: openvms
    14: nonstop_kernel
    15: ar_os
    16: fuchsia
    17: cloud_abi
    18: openvos

  pt_type:
    0: "null"
    1: "load"
    2: "dynamic"
    3: "interp"
    4: "note"
    5: "shlib"
    6: "phdr"
    0x60000000: "loos"
    0x6fffffff: "hios"
    0x70000000: "loproc"
    0x7fffffff: "hiproc"

  sht_type:
    0: "null"
    1: "progbits"
    2: "symtab"
    3: "strtab"
    4: "rela"
    5: "hash"
    6: "dynamic"
    7: "note"
    8: "nobits"
    9: "rel"
    10: "shlib"
    11: "dynsym"
    14: "init_array"
    15: "fini_array"
    16: "preinit_array"
    17: "group"
    18: "symtab_shndx"
    0x60000000: "loos"
    0x6fffffff: "hios"
    0x70000000: "loproc"
    0x7fffffff: "hiproc"