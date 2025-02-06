meta:
  id: elf
  title: Executable and Linkable Format (ELF)
  file-extension: elf
  endian: be
  license: CC0-1.0

seq:
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident
        type: e_ident
      - id: e_type
        type: u2
      - id: e_machine
        type: u2
      - id: e_version
        type: u4
      - id: e_entry
        type: u4
      - id: e_phoff
        type: u4
      - id: e_shoff
        type: u4
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
        contents: [0x7f, 0x45, 0x4c, 0x46]
      - id: class
        type: u1
        enum: elf_class
      - id: data
        type: u1
        enum: elf_data
      - id: version
        type: u1
      - id: osabi
        type: u1
        enum: elf_osabi
      - id: abiversion
        type: u1
      - id: pad
        size: 7

  program_header:
    seq:
      - id: p_type
        type: u4
        enum: segment_type
      - id: p_offset
        type: u4
      - id: p_vaddr
        type: u4
      - id: p_paddr
        type: u4
      - id: p_filesz
        type: u4
      - id: p_memsz
        type: u4
      - id: p_flags
        type: u4
      - id: p_align
        type: u4

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
        enum: section_type
      - id: sh_flags
        type: u4
      - id: sh_addr
        type: u4
      - id: sh_offset
        type: u4
      - id: sh_size
        type: u4
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u4
      - id: sh_entsize
        type: u4

enums:
  elf_class:
    1: elfclass32
    2: elfclass64

  elf_data:
    1: elfdata2lsb
    2: elfdata2msb

  elf_osabi:
    0: elfosabi_sysv
    3: elfosabi_linux

  segment_type:
    0: pt_null
    1: pt_load
    2: pt_dynamic
    3: pt_interp
    4: pt_note
    5: pt_shlib
    6: pt_phdr
    7: pt_tls

  section_type:
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