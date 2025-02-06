meta:
  id: elf
  endian: le
  title: ELF (Executable and Linkable Format)
  file-extension: elf
types:
  Elf_Phdr:
    seq:
      - id: type
        size: 4
        type: u4
      - id: offset
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: offset_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: vaddr
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: vaddr_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: paddr
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: paddr_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: filesz
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: filesz_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: memsz
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: memsz_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: flags
        size: 4
        type: u4
      - id: align
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: align_64
        size: 8
        type: u8
        if: _root.header.class == 2
  Elf_Shdr:
    seq:
      - id: name
        size: 4
        type: u4
      - id: type
        size: 4
        type: u4
      - id: flags
        size: 8
        type: u8
      - id: addr
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: addr_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: offset
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: offset_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: size
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: size_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: link
        size: 4
        type: u4
      - id: info
        size: 4
        type: u4
      - id: addralign
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: addralign_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: entsize
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: entsize_64
        size: 8
        type: u8
        if: _root.header.class == 2
  Elf_Sym:
    seq:
      - id: name
        size: 4
        type: u4
      - id: value
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: value_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: size
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: size_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: info
        size: 1
        type: u1
      - id: other
        size: 1
        type: u1
      - id: shndx
        size: 2
        type: u2
  Elf_Rel:
    seq:
      - id: offset
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: offset_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: info
        size: 4
        type: u4
      - id: addend
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: addend_64
        size: 8
        type: u8
        if: _root.header.class == 2
  Elf_Rela:
    seq:
      - id: offset
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: offset_64
        size: 8
        type: u8
        if: _root.header.class == 2
      - id: info
        size: 4
        type: u4
      - id: addend
        size: 4
        type: u4
        if: _root.header.class == 1
      - id: addend_64
        size: 8
        type: u8
        if: _root.header.class == 2
  Elf_Hdr:
    seq:
      - id: magic
        size: 4
        type: str
        encoding: ascii
      - id: class
        size: 1
        type: u1
      - id: data
        size: 1
        type: u1
      - id: version
        size: 1
        type: u1
      - id: os_abi
        size: 1
        type: u1
      - id: abi_version
        size: 1
        type: u1
      - id: pad
        size: 7
        type: str
        encoding: ascii
      - id: type
        size: 2
        type: u2
      - id: machine
        size: 2
        type: u2
      - id: version_2
        size: 4
        type: u4
      - id: entry
        size: 4
        type: u4
        if: class == 1
      - id: entry_64
        size: 8
        type: u8
        if: class == 2
      - id: phoff
        size: 4
        type: u4
        if: class == 1
      - id: phoff_64
        size: 8
        type: u8
        if: class == 2
      - id: shoff
        size: 4
        type: u4
        if: class == 1
      - id: shoff_64
        size: 8
        type: u8
        if: class == 2
      - id: flags
        size: 4
        type: u4
      - id: ehsize
        size: 2
        type: u2
      - id: phentsize
        size: 2
        type: u2
      - id: phnum
        size: 2
        type: u2
      - id: shentsize
        size: 2
        type: u2
      - id: shnum
        size: 2
        type: u2
      - id: shstrndx
        size: 2
        type: u2
seq:
  - id: header
    type: Elf_Hdr
instances:
  phdrs:
    type: Elf_Phdr
    pos: header.phoff
    repeat: header.phnum
    if: header.class == 1
  phdrs_64:
    type: Elf_Phdr
    pos: header.phoff_64
    repeat: header.phnum
    if: header.class == 2
  shdrs:
    type: Elf_Shdr
    pos: header.shoff
    repeat: header.shnum
    if: header.class == 1
  shdrs_64:
    type: Elf_Shdr
    pos: header.shoff_64
    repeat: header.shnum
    if: header.class == 2