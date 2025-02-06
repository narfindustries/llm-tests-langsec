meta:
  id: elf
  title: ELF (Executable and Linkable Format)
  file-extension: elf
  imports:
    - /types/elf_ident.ksy

seq:
  - id: header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident
        type: ident
      - id: e_type
        type: u2
        process: switch_endian
      - id: e_machine
        type: u2
        process: switch_endian
      - id: e_version
        type: u4
        process: switch_endian
      - id: e_entry
        type: u4
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: e_entry_64
        type: u8
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: e_phoff
        type: u4
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: e_phoff_64
        type: u8
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: e_shoff
        type: u4
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: e_shoff_64
        type: u8
        process: switch_endian
        if: e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: e_flags
        type: u4
        process: switch_endian
      - id: e_ehsize
        type: u2
        process: switch_endian
      - id: e_phentsize
        type: u2
        process: switch_endian
      - id: e_phnum
        type: u2
        process: switch_endian
      - id: e_shentsize
        type: u2
        process: switch_endian
      - id: e_shnum
        type: u2
        process: switch_endian
      - id: e_shstrndx
        type: u2
        process: switch_endian

  ident:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46]
      - id: ei_class
        type: u1
      - id: ei_data
        type: u1
      - id: ei_version
        type: u1
      - id: ei_osabi
        type: u1
      - id: ei_abiversion
        type: u1
      - id: ei_pad
        size: 7

  program_header:
    seq:
      - id: p_type
        type: u4
        process: switch_endian
      - id: p_offset
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_offset_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: p_vaddr
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_vaddr_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: p_paddr
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_paddr_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: p_filesz
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_filesz_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: p_memsz
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_memsz_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: p_flags
        type: u4
        process: switch_endian
      - id: p_align
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: p_align_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64

  section_header:
    seq:
      - id: sh_name
        type: u4
        process: switch_endian
      - id: sh_type
        type: u4
        process: switch_endian
      - id: sh_flags
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_flags_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: sh_addr
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_addr_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: sh_offset
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_offset_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: sh_size
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_size_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: sh_link
        type: u4
        process: switch_endian
      - id: sh_info
        type: u4
        process: switch_endian
      - id: sh_addralign
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_addralign_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: sh_entsize
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: sh_entsize_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64

  symbol:
    seq:
      - id: st_name
        type: u4
        process: switch_endian
      - id: st_value
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: st_value_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: st_size
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: st_size_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: st_info
        type: u1
      - id: st_other
        type: u1
      - id: st_shndx
        type: u2
        process: switch_endian

  relocation:
    seq:
      - id: r_offset
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: r_offset_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: r_info
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: r_info_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64
      - id: r_addend
        type: u4
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass32
      - id: r_addend_64
        type: u8
        process: switch_endian
        if: _root.header.e_ident.ei_class == elf_ident.ei_class_t.elfclass64

processes:
  switch_endian:
    - if: _root.header.e_ident.ei_data == 1
      type: byteswap
      endian: le
    - if: _root.header.e_ident.ei_data == 2
      type: byteswap
      endian: be