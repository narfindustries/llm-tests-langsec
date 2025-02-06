meta:
  id: elf
  title: Executable and Linkable Format
  file-extension: elf
  license: CC0-1.0

seq:
  - id: elf_header
    type: elf_header

types:
  elf_header:
    seq:
      - id: e_ident
        type: e_ident
      - id: e_type
        type: u2
        endian: _root.endian
      - id: e_machine
        type: u2
        endian: _root.endian
      - id: e_version
        type: u4
        endian: _root.endian
      - id: e_entry
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: e_phoff
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: e_shoff
        type:
          switch-on: e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: e_flags
        type: u4
        endian: _root.endian
      - id: e_ehsize
        type: u2
        endian: _root.endian
      - id: e_phentsize
        type: u2
        endian: _root.endian
      - id: e_phnum
        type: u2
        endian: _root.endian
      - id: e_shentsize
        type: u2
        endian: _root.endian
      - id: e_shnum
        type: u2
        endian: _root.endian
      - id: e_shstrndx
        type: u2
        endian: _root.endian

  e_ident:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46]
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
        enum: segment_type
        endian: _root.endian
      - id: p_offset
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: p_vaddr
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: p_paddr
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: p_filesz
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: p_memsz
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: p_flags
        type: u4
        endian: _root.endian
      - id: p_align
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian

  section_header:
    seq:
      - id: sh_name
        type: u4
        endian: _root.endian
      - id: sh_type
        type: u4
        enum: section_type
        endian: _root.endian
      - id: sh_flags
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: sh_addr
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: sh_offset
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: sh_size
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: sh_link
        type: u4
        endian: _root.endian
      - id: sh_info
        type: u4
        endian: _root.endian
      - id: sh_addralign
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian
      - id: sh_entsize
        type:
          switch-on: _root.elf_header.e_ident.ei_class
          cases:
            'elf_class::class_32': u4
            'elf_class::class_64': u8
        endian: _root.endian

instances:
  endian:
    value: 'e_ident.ei_data == elf_data::data2lsb ? "le" : "be"'

enums:
  elf_class:
    1: class_32
    2: class_64

  elf_data:
    1: data2lsb
    2: data2msb

  elf_osabi:
    0: sysv
    1: hpux
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

  segment_type:
    0: null_type
    1: load
    2: dynamic
    3: interp
    4: note
    5: shlib
    6: phdr
    7: tls
    0x60000000: loos
    0x6FFFFFFF: hios
    0x70000000: loproc
    0x7FFFFFFF: hiproc

  section_type:
    0: null_type
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
    0x60000000: loos
    0x6FFFFFFF: hios
    0x70000000: loproc
    0x7FFFFFFF: hiproc
    0x80000000: louser
    0xFFFFFFFF: hiuser