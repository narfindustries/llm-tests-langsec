meta:
  id: elf
  file-extension: elf
  endian: le
seq:
  - id: m
    type: elf__header
types:
  elf__header:
    seq:
      - id: magic
        contents: [0x7F, 'E', 'L', 'F']
      - id: bitness
        type: u1
        enum: bitness
      - id: endianness
        type: u1
        enum: endianness
      - id: elf_version
        type: u1
      - id: abi
        type: u1
        enum: abi
      - id: abi_version
        type: u1
      - id: padding
        size: 7
      - id: e_type
        type: u2
        enum: type
      - id: e_machine
        type: u2
        enum: machine
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
  elf__program_header:
    seq:
      - id: p_type
        type: u4
        enum: p_type
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
        enum: p_flags
      - id: p_align
        type: u4
  elf__section_header:
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
  elf__symbol:
    seq:
      - id: st_name
        type: u4
      - id: st_value
        type: u4
      - id: st_size
        type: u4
      - id: st_info
        type: u1
      - id: st_other
        type: u1
      - id: st_shndx
        type: u2
enums:
  bitness:
    1: elf32
    2: elf64
  endianness:
    1: le
    2: be
  abi:
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
  type:
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
    6: em_860
    7: em_mips
    8: em_parisc
    9: em_sparc32plus
    10: em_ppc
    11: em_ppc64
    12: em_s390
    13: em_arm
    14: em_sh
    15: em_sparcv9
    16: em_ia_64
    17: em_x86_64
    18: em_vax
  p_type:
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
  p_flags:
    1: pf_x
    2: pf_w
    4: pf_r
  sh_type:
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
    14: sht_init_array
    15: sht_fini_array
    16: sht_preinit_array
    17: sht_group
    18: sht_symtab_shndx
    0x70000000: sht_loos
    0x7fffffff: sht_hios
    0x80000000: sht_loproc
    0x8fffffff: sht_hiproc
  sh_flags:
    1: shf_write
    2: shf_alloc
    4: shf_execinstr
    16: shf_merge
    32: shf_strings
    64: shf_info_link
    128: shf_link_order
    256: shf_os_nonconforming
    512: shf_group
    1024: shf_tls
    0x0ff00000: shf_maskos
    0xf0000000: shf_maskproc
instances:
  program_headers:
    type: elf__program_header
    repeat: expr
    repeat-expr: e_phnum
    io: _root._io
    pos: e_phoff
  section_headers:
    type: elf__section_header
    repeat: expr
    repeat-expr: e_shnum
    io: _root._io
    pos: e_shoff
  strings:
    type: str
    encoding: UTF-8
    size-eos: true
    io: _root._io
    pos: section_headers[e_shstrndx].sh_offset
  symbols:
    type: elf__symbol
    repeat: expr
    repeat-expr: section_headers[sections['.symtab']].sh_size / section_headers[sections['.symtab']].sh_entsize
    io: _root._io
    pos: section_headers[sections['.symtab']].sh_offset
  sections:
    type: str
    encoding: UTF-8
    size: e_shnum
    pos: e_shoff
    io: _root._io