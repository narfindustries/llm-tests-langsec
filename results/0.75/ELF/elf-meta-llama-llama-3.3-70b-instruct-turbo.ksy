meta:
  id: elf
  file-extension: elf
  endian: le

seq:
  - id: e_ident
    size: 16
    type: str
    doc: |
      The ELF identification bytes.

  - id: e_type
    size: 2
    type: elf_class

  - id: e_machine
    size: 2
    type: elf_machine

  - id: e_version
    size: 4
    type: u4

  - id: e_entry
    size: 4
    type: u4
    if: e_ident.ei_class == elf_class.elf32
    doc: |
      The virtual address to which the system first transfers control, thus starting the process.

  - id: e_entry_64
    size: 8
    type: u8
    if: e_ident.ei_class == elf_class.elf64
    doc: |
      The virtual address to which the system first transfers control, thus starting the process.

  - id: e_phoff
    size: 4
    type: u4
    if: e_ident.ei_class == elf_class.elf32
    doc: |
      This member holds the memory offset of the segment header table.

  - id: e_phoff_64
    size: 8
    type: u8
    if: e_ident.ei_class == elf_class.elf64
    doc: |
      This member holds the memory offset of the segment header table.

  - id: e_shoff
    size: 4
    type: u4
    if: e_ident.ei_class == elf_class.elf32
    doc: |
      This member holds the memory offset of the section header table.

  - id: e_shoff_64
    size: 8
    type: u8
    if: e_ident.ei_class == elf_class.elf64
    doc: |
      This member holds the memory offset of the section header table.

  - id: e_flags
    size: 4
    type: u4
    doc: |
      This member holds processor-specific flags associated with the file.

  - id: e_ehsize
    size: 2
    type: u2
    doc: |
      This member holds the size of this header, including the e_ident fields.

  - id: e_phentsize
    size: 2
    type: u2
    doc: |
      This member holds the size of a program header table entry.

  - id: e_phnum
    size: 2
    type: u2
    doc: |
      This member holds the number of entries in the program header table.

  - id: e_shentsize
    size: 2
    type: u2
    doc: |
      This member holds the size of a section header entry.

  - id: e_shnum
    size: 2
    type: u2
    doc: |
      This member holds the number of entries in the section header table.

  - id: e_shstrndx
    size: 2
    type: u2
    doc: |
      This member holds the section header table index of the entry that contains the section name string table.

types:
  elf_class:
    enum:
      elf32: 1
      elf64: 2

  elf_machine:
    enum:
      em_none: 0
      em_m32: 1
      em_sparc: 2
      em_386: 3
      em_68k: 4
      em_88k: 5
      em_486: 6
      em_860: 7
      em_mips: 8
      em_s370: 9
      em_mipsrs3le: 10
      em_parisc: 15
      em_vpp500: 17
      em_sparc32plus: 18
      em_960: 19
      em_ppc: 20
      em_ppc64: 21
      em_s390: 22
      em_spu: 23
      em_v800: 36
      em_fr20: 37
      em_rh32: 38
      em_rce: 39
      em_arm: 40
      em_alpha: 41
      em_sh: 42
      em_sparcv9: 43
      em_tricore: 44
      em_arc: 45
      em_h8_300: 46
      em_h8s: 48
      em_h9: 49
      em_kn02: 50
      em_m32r: 51
      em_mn10200: 53
      em_pj: 54
      em_openrisc: 92
      em_arm_arse: 183
      em_tmm_gpp: 216
      em_ns32k: 241
      em_tpc: 242
      em_snp1k: 243
      em_st200: 244
      em_ut_mips: 245
      em_68hc12: 247
      em_mma: 248
      em_pcp: 249
      em_ncpu: 250
      em_ndr1: 251
      em_starcore: 252
      em_me16: 253
      em_st100: 254
      em_tinyj: 255
      em_x86_64: 62
      em_aeabi: 293
      em_z80: 94
      em_cr16: 134
      em_cr16c: 135
      em_etpu: 136
      em_sle9x: 137
      em_crx: 138
      em_m16c: 243
      em_dx10: 244
      em_d10v: 245
      em_d30v: 246
      em_m32c: 247
      em_mx: 249
      em_h8s: 251
      em_m16c: 252
      em_k10: 253
      em_k8: 254
      em_k12: 255
      em_aarch64: 183
      em_arm: 40
      em_x86: 3
      em_mips: 8
      em_mips_x: 101
      em_m32r: 88
      em_pj: 91
      em_openrisc: 92
      em_arc_compact: 93
      em_xtensa: 94
      em_num: 243

  e_ident:
    seq:
      - id: ei_mag
        size: 4
        type: str
        doc: |
          The ELF magic number.

      - id: ei_class
        size: 1
        type: u1
        doc: |
          The ELF class.

      - id: ei_data
        size: 1
        type: u1
        doc: |
          The ELF data encoding.

      - id: ei_version
        size: 1
        type: u1
        doc: |
          The ELF version.

      - id: ei_osabi
        size: 1
        type: u1
        doc: |
          The ELF OS ABI.

      - id: ei_abiversion
        size: 1
        type: u1
        doc: |
          The ELF ABI version.

      - id: ei_pad
        size: 7
        type: str
        doc: |
          The ELF padding.

  program_header:
    seq:
      - id: p_type
        size: 4
        type: u4
        doc: |
          The segment type.

      - id: p_offset
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The offset from the beginning of the file at which the first byte of the segment resides.

      - id: p_offset_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The offset from the beginning of the file at which the first byte of the segment resides.

      - id: p_vaddr
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The virtual address at which the first byte of the segment resides.

      - id: p_vaddr_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The virtual address at which the first byte of the segment resides.

      - id: p_paddr
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          On systems for which physical addressing is relevant, this member is reserved for the segment’s physical address.

      - id: p_paddr_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          On systems for which physical addressing is relevant, this member is reserved for the segment’s physical address.

      - id: p_filesz
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The number of bytes in the file image of the segment.

      - id: p_filesz_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The number of bytes in the file image of the segment.

      - id: p_memsz
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The number of bytes in the memory image of the segment.

      - id: p_memsz_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The number of bytes in the memory image of the segment.

      - id: p_flags
        size: 4
        type: u4
        doc: |
          This member holds one-bit flags associated with the segment.

      - id: p_align
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The value to which the segments are aligned in memory and in the file.

      - id: p_align_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The value to which the segments are aligned in memory and in the file.

  section_header:
    seq:
      - id: sh_name
        size: 4
        type: u4
        doc: |
          This member specifies the section name.

      - id: sh_type
        size: 4
        type: u4
        doc: |
          This member categorizes the section’s contents and semantics.

      - id: sh_flags
        size: 4
        type: u4
        doc: |
          Sections support one-bit flags that describe miscellaneous attributes.

      - id: sh_addr
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          If the section will reside in memory, this member holds the address at which the section’s first byte should reside.

      - id: sh_addr_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          If the section will reside in memory, this member holds the address at which the section’s first byte should reside.

      - id: sh_offset
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member’s value holds the offset from the beginning of the file to the first byte in the section.

      - id: sh_offset_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member’s value holds the offset from the beginning of the file to the first byte in the section.

      - id: sh_size
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member holds the section’s size in bytes.

      - id: sh_size_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member holds the section’s size in bytes.

      - id: sh_link
        size: 4
        type: u4
        doc: |
          This member holds a section header table index that gives a related section.

      - id: sh_info
        size: 4
        type: u4
        doc: |
          This member holds extra information, whose interpretation depends on the section type.

      - id: sh_addralign
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          Some sections have address alignment constraints.

      - id: sh_addralign_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          Some sections have address alignment constraints.

      - id: sh_entsize
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member holds a section’s entry size if the section holds a table.

      - id: sh_entsize_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member holds a section’s entry size if the section holds a table.

  symbol_table:
    seq:
      - id: st_name
        size: 4
        type: u4
        doc: |
          An index into the section header string table section that gives the name of the symbol.

      - id: st_value
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          The value of the associated symbol.

      - id: st_value_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          The value of the associated symbol.

      - id: st_size
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          Many symbols have associated sizes.

      - id: st_size_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          Many symbols have associated sizes.

      - id: st_info
        size: 1
        type: u1
        doc: |
          This member specifies the symbol’s type and binding attributes.

      - id: st_other
        size: 1
        type: u1
        doc: |
          This member currently holds no defined meaning with respect to the section header table.

      - id: st_shndx
        size: 2
        type: u2
        doc: |
          Every symbol table entry is defined as “local” or “global”.

  relocation:
    seq:
      - id: r_offset
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member gives the location at which to apply the relocation action.

      - id: r_offset_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member gives the location at which to apply the relocation action.

      - id: r_info
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member gives both the symbol table index and the type of relocation to apply.

      - id: r_info_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member gives both the symbol table index and the type of relocation to apply.

      - id: r_addend
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member specifies a constant addend used to compute the value to be stored into the relocatable field.

      - id: r_addend_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member specifies a constant addend used to compute the value to be stored into the relocatable field.

  dynamic:
    seq:
      - id: d_tag
        size: 4
        type: u4
        doc: |
          This member’s value can have the following values.

      - id: d_un
        size: 4
        type: u4
        if: e_ident.ei_class == elf_class.elf32
        doc: |
          This member’s interpretation depends on the value of d_tag.

      - id: d_un_64
        size: 8
        type: u8
        if: e_ident.ei_class == elf_class.elf64
        doc: |
          This member’s interpretation depends on the value of d_tag.

instances:
  program_headers:
    type: program_header
    repeat: e_phnum
    doc: |
      The program header table is an array of structures, each of which describes a program segment or other information that the system needs to prepare the program for execution.