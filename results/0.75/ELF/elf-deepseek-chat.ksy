meta:
  id: elf_deepseek_chat
  file-extension: elf
  endian: le
  license: MIT
  imports:
    - /common/elf

seq:
  - id: header
    type: elf.elf_header
  - id: sections
    type: section_header
    repeat: expr
    repeat-expr: header.section_header_entry_count
  - id: segments
    type: program_header
    repeat: expr
    repeat-expr: header.program_header_entry_count
  - id: section_names
    type: strz
    size: header.section_header_entry_size
    repeat: expr
    repeat-expr: header.section_header_entry_count

types:
  section_header:
    seq:
      - id: name_offset
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

  program_header:
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

  strz:
    seq:
      - id: value
        type: str
        encoding: UTF-8
        size-eos: true