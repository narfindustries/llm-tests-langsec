meta:
  id: elf
  endian: be
types:
  elf_ident:
    seq:
      - id: magic
        type: u4
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
        type: u1
        size: 7
  elf_header:
    seq:
      - id: ident
        type: elf_ident
      - id: type
        type: u2
      - id: machine
        type: u2
      - id: version
        type: u4
      - id: entry
        type: u4
      - id: phoff
        type: u4
      - id: shoff
        type: u4
      - id: flags
        type: u4
      - id: ehsize
        type: u2
      - id: phentsize
        type: u2
      - id: phnum
        type: u2
      - id: shentsize
        type: u2
      - id: shnum
        type: u2
      - id: shstrndx
        type: u2
  elf_program_header:
    seq:
      - id: type
        type: u4
      - id: offset
        type: u4
      - id: vaddr
        type: u4
      - id: paddr
        type: u4
      - id: filesz
        type: u4
      - id: memsz
        type: u4
      - id: flags
        type: u4
      - id: align
        type: u4
  elf_section_header:
    seq:
      - id: name
        type: u4
      - id: type
        type: u4
      - id: flags
        type: u4
      - id: addr
        type: u4
      - id: offset
        type: u4
      - id: size
        type: u4
      - id: link
        type: u4
      - id: info
        type: u4
      - id: addralign
        type: u4
      - id: entsize
        type: u4

seq:
  - id: header
    type: elf_header
  - id: program_headers
    type: elf_program_header
    repeat: expr
    repeat-expr: header.phnum
  - id: section_headers
    type: elf_section_header
    repeat: expr
    repeat-expr: header.shnum

