meta:
  id: elf
  title: Executable and Linkable Format (ELF)
  license: MIT
  endian: be
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
types:
  e_ident:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_class
      - id: data
        type: u1
        enum: elf_data
      - id: version
        type: u1
        enum: elf_version
      - id: osabi
        type: u1
        enum: elf_osabi
      - id: abiversion
        type: u1
      - id: pad
        size: 7
  elf_class:
    enum:
      0: none
      1: elf32
      2: elf64
  elf_data:
    enum:
      0: none
      1: lsb
      2: msb
  elf_version:
    enum:
      0: none
      1: current
  elf_osabi:
    enum:
      0: system_v
      1: hp_ux
      2: netbsd
      3: linux
      4: gnu_hurd
      5: solaris
      6: aix
      7: irix
      8: freebsd
      9: tru64
      10: novell_modesto
      11: openbsd
      12: openvms
      13: nonstop_kernel
      14: aros
      15: fenix_os
      16: cloudabi
      97: arm
      255: standalone
  program_header:
    seq:
      - id: p_type
        type: u4
        enum: p_type
      - id: p_flags
        type: u4
        enum: p_flags
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
      - id: p_align
        type: u4
  p_type:
    enum:
      0: null
      1: load
      2: dynamic
      3: interp
      4: note
      5: shlib
      6: phdr
      7: tls
  p_flags:
    enum:
      1: x
      2: w
      4: r
  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
        enum: sh_type
      - id: sh_flags
        type: u4
        enum: sh_flags
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
  sh_type:
    enum:
      0: null
      1: progbits
      2: symtab
      3: strtab
      4: rela
      5: hash
      6: dynamic
      7: note
      8: nobits
      9: rel
      10: shlib
      11: dynsym
      14: init_array
      15: fini_array
      16: preinit_array
      17: group
      18: symtab_shndx
      19: num
  sh_flags:
    enum:
      1: write
      2: alloc
      4: execinstr
      16: merge
      32: strings
      64: info_link
      128: link_order
      256: os_nonconforming
      512: group
      1024: tls
  symbol:
    seq:
      - id: st_name
        type: u4
      - id: st_value
        type: u4
      - id: st_size
        type: u4
      - id: st_info
        type: u1
        enum: st_info
      - id: st_other
        type: u1
        enum: st_other
      - id: st_shndx
        type: u2
  st_info:
    enum:
      0: notype
      1: object
      2: func
      3: section
      4: file
      13: loos
      15: hios
      16: loproc
      17: hiproc
  st_other:
    enum:
      0: default
      1: internal
      2: hidden
      3: protected
  relocation:
    seq:
      - id: r_offset
        type: u4
      - id: r_info
        type: u4
      - id: r_addend
        type: u4
instances:
  program_headers:
    type: program_header
    repeat: expr
    repeat-expr: e_phnum
    pos: e_phoff
  section_headers:
    type: section_header
    repeat: expr
    repeat-expr: e_shnum
    pos: e_shoff
  shstrtab:
    type: strtab
    pos: section_headers[e_shstrndx].sh_offset
    size: section_headers[e_shstrndx].sh_size
  strtab:
    seq:
      - id: strings
        type: strz
        encoding: ASCII
        size: section_headers[sh_link].sh_size
  symtab:
    type: symbol
    repeat: expr
    repeat-expr: section_headers[sh_link].sh_size / section_headers[sh_link].sh_entsize
    pos: section_headers[sh_link].sh_offset
  relocations:
    type: relocation
    repeat: expr
    repeat-expr: section_headers[sh_link].sh_size / section_headers[sh_link].sh_entsize
    pos: section_headers[sh_link].sh_offset