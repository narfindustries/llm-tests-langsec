meta:
  id: elf
  endian: be
  title: ELF (Executable and Linkable Format)
  file-extension: elf
  license: CC0-1.0
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
    type:
      switch-on: e_ident.ei_class
      cases:
        'elf_class_t::elfclass32': u4
        'elf_class_t::elfclass64': u8
  - id: e_phoff
    type:
      switch-on: e_ident.ei_class
      cases:
        'elf_class_t::elfclass32': u4
        'elf_class_t::elfclass64': u8
  - id: e_shoff
    type:
      switch-on: e_ident.ei_class
      cases:
        'elf_class_t::elfclass32': u4
        'elf_class_t::elfclass64': u8
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
    if: e_phnum > 0
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: e_shnum
    if: e_shnum > 0

types:
  e_ident:
    seq:
      - id: ei_mag
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
      - id: p_flags
        type: u4
        if: e_ident.ei_class == elf_class_t::elfclass64
      - id: p_offset
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: p_vaddr
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: p_paddr
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: p_filesz
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: p_memsz
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: p_align
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8

  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
      - id: sh_flags
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: sh_addr
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: sh_offset
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: sh_size
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8
      - id: sh_entsize
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class_t::elfclass32': u4
            'elf_class_t::elfclass64': u8

enums:
  elf_class_t:
    1: elfclass32
    2: elfclass64

  elf_data_t:
    1: data2lsb
    2: data2msb

  elf_osabi_t:
    0: sysv
    1: hpux
    2: netbsd
    3: linux
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: novell_modesto
    12: openbsd
    13: openvms
    14: nsk
    15: aros
    97: arm_aeabi
    255: standalone

  object_file_type_t:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core

  machine_t:
    0: none
    1: m32
    2: sparc
    3: i386
    4: m68k
    5: m88k
    6: i860
    7: mips
    8: s370
    10: mips_rs3_le
    15: parisc
    17: vpp500
    18: sparc32plus
    20: i960
    21: ppc
    22: ppc64
    36: arm
    40: ia_64
    42: alpha
    43: sparcv9
    50: hppa
    62: amd64
    183: aarch64

  segment_type_t:
    '0': null_type
    '1': load
    '2': dynamic
    '3': interp
    '4': note
    '5': shlib
    '6': phdr
    '7': tls

  section_type_t:
    '0': null_type
    '1': progbits
    '2': symtab
    '3': strtab
    '4': rela
    '5': hash
    '6': dynamic
    '7': note
    '8': nobits
    '9': rel
    '10': shlib
    '11': dynsym
    '14': init_array
    '15': fini_array
    '16': preinit_array
    '17': group
    '18': symtab_shndx

  section_flags_t:
    1: write
    2: alloc
    4: execinstr