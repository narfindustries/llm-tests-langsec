meta:
  id: elf
  title: Executable and Linkable Format (ELF)
  file-extension: elf
  endian: be

seq:
  - id: header
    type: header

types:
  header:
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
      - id: abiversion
        type: u1
      - id: pad
        size: 7

enums:
  elf_class:
    0: none
    1: class32
    2: class64

  elf_data:
    0: none
    1: lsb
    2: msb