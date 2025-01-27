meta:
  id: elf
  file-extension: elf
  endian: le
  title: Elf File Format
  license: MIT
seq:
  - id: e_ident
    type: elf_ident
    size: 16
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
  elf_ident:
    seq:
      - id: magic
        type: str
        size: 4
        encoding: ASCII
      - id: class
        type: u1
      - id: data
        type: u1
      - id: version
        type: u1
      - id: osabi
        type: u1
      - id: abiversion
        type: u1
      - id: pad
        type: str
        size: 7
        encoding: ASCII