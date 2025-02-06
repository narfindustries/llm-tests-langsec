meta:
  id: elf
  title: ELF (Executable and Linkable Format)
  endian: 
    switch-on: header.e_ident.ei_data
    cases:
      1: le
      2: be

seq:
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident
        type: ident_struct
      - id: e_type
        type: u2
        enum: elf_object_type
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

  ident_struct:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46]
      - id: ei_class
        type: u1
        enum: elf_class
      - id: ei_data
        type: u1
        enum: elf_data_encoding
      - id: ei_version
        type: u1
        enum: elf_version
      - id: ei_osabi
        type: u1
        enum: elf_os_abi
      - id: ei_abiversion
        type: u1
      - id: ei_pad
        size: 7

  program_header:
    seq:
      - id: p_type
        type: u4
        enum: elf_p_type
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
        enum: elf_sh_type
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
    0: elfclassnone
    1: elfclass32
    2: elfclass64

  elf_data_encoding:
    0: elfdatanone
    1: elfdata2lsb
    2: elfdata2msb

  elf_version:
    0: ev_none
    1: ev_current

  elf_os_abi:
    0: elfosabi_sysv
    3: elfosabi_linux

  elf_object_type:
    0: et_none
    1: et_rel
    2: et_exec
    3: et_dyn
    4: et_core

  elf_machine:
    0: em_none
    2: em_sparc
    3: em_386
    8: em_mips
    62: em_x86_64

  elf_p_type:
    0: pt_null
    1: pt_load
    2: pt_dynamic
    3: pt_interp
    4: pt_note
    5: pt_shlib
    6: pt_phdr

  elf_sh_type:
    0: sht_null
    1: sht_progbits
    2: sht_symtab
    3: sht_strtab
    4: sht_rela
    5: sht_hash
    6: sht_dynamic
    7: sht_note
    8: sht_nobits
    9: sht_rel
    10: sht_shlib
    11: sht_dynsym