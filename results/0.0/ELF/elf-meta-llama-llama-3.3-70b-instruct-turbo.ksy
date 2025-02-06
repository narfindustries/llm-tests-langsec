meta:
  id: elf
  endian: le
seq:
  - id: e_ident
    type: e_ident
  - id: e_type
    type: uint2
  - id: e_machine
    type: uint2
  - id: e_version
    type: uint4
  - id: e_entry
    type: uint_e_entry
  - id: e_phoff
    type: uint_e_phoff
  - id: e_shoff
    type: uint_e_shoff
  - id: e_flags
    type: uint4
  - id: e_ehsize
    type: uint2
  - id: e_phentsize
    type: uint2
  - id: e_phnum
    type: uint2
  - id: e_shentsize
    type: uint2
  - id: e_shnum
    type: uint2
  - id: e_shstrndx
    type: uint2
types:
  e_ident:
    seq:
      - id: ei_mag
        size: 4
      - id: ei_class
        type: uint1
      - id: ei_data
        type: uint1
      - id: ei_version
        type: uint1
      - id: ei_osabi
        type: uint1
      - id: ei_abiversion
        type: uint1
      - id: ei_pad
        size: 7
  uint_e_entry:
    type:
      if: e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_e_phoff:
    type:
      if: e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_e_shoff:
    type:
      if: e_ident.ei_class == 1
        then: uint4
      else: uint8
  program_header:
    seq:
      - id: p_type
        type: uint4
      - id: p_offset
        type: uint_p_offset
      - id: p_vaddr
        type: uint_p_vaddr
      - id: p_paddr
        type: uint_p_paddr
      - id: p_filesz
        type: uint_p_filesz
      - id: p_memsz
        type: uint_p_memsz
      - id: p_flags
        type: uint4
      - id: p_align
        type: uint_p_align
  uint_p_offset:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_p_vaddr:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_p_paddr:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_p_filesz:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_p_memsz:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_p_align:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  section_header:
    seq:
      - id: sh_name
        type: uint4
      - id: sh_type
        type: uint4
      - id: sh_flags
        type: uint4
      - id: sh_addr
        type: uint_sh_addr
      - id: sh_offset
        type: uint_sh_offset
      - id: sh_size
        type: uint_sh_size
      - id: sh_link
        type: uint4
      - id: sh_info
        type: uint4
      - id: sh_addralign
        type: uint_sh_addralign
      - id: sh_entsize
        type: uint_sh_entsize
  uint_sh_addr:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_sh_offset:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_sh_size:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_sh_addralign:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_sh_entsize:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  symbol_table:
    seq:
      - id: st_name
        type: uint4
      - id: st_value
        type: uint_st_value
      - id: st_size
        type: uint_st_size
      - id: st_info
        type: uint1
      - id: st_other
        type: uint1
      - id: st_shndx
        type: uint2
  uint_st_value:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_st_size:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  dynamic_section:
    seq:
      - id: d_tag
        type: uint4
      - id: d_un
        type: uint_d_un
  uint_d_un:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  relocation_section:
    seq:
      - id: r_offset
        type: uint_r_offset
      - id: r_info
        type: uint_r_info
      - id: r_addend
        type: uint_r_addend
  uint_r_offset:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_r_info:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
  uint_r_addend:
    type:
      if: _root.e_ident.ei_class == 1
        then: uint4
      else: uint8
instances:
  program_headers:
    type: program_header
    repeat: expr
    repeat-expr: e_phnum
  section_headers:
    type: section_header
    repeat: expr
    repeat-expr: e_shnum
  symbol_tables:
    type: symbol_table
    repeat: expr
    repeat-expr: section_headers[e_shstrndx].sh_size / section_headers[e_shstrndx].sh_entsize
  dynamic_sections:
    type: dynamic_section
    repeat: expr
    repeat-expr: section_headers[e_shstrndx].sh_size / section_headers[e_shstrndx].sh_entsize
  relocation_sections:
    type: relocation_section
    repeat: expr
    repeat-expr: section_headers[e_shstrndx].sh_size / section_headers[e_shstrndx].sh_entsize