meta:
  id: elf-gemini-1
  title: ELF Gemini 1.5 Flash
  doc: >
    Kaitai Struct specification for the ELF file format used in Gemini 1.5 flash memory.  This is a simplified representation and may not cover all edge cases.

types:
  elf_header:
    seq:
      - id: magic
        type: u4
        doc: Magic number (0x7f454c46)
      - id: class
        type: u1
        doc: File class (ELFCLASS32 or ELFCLASS64)
      - id: endianness
        type: u1
        doc: Data encoding (ELFDATA2LSB or ELFDATA2MSB)
      - id: version
        type: u1
        doc: File version (usually 1)
      - id: osabi
        type: u1
        doc: OS ABI identification
      - id: abiversion
        type: u1
        doc: ABI version
      - id: pad
        type: u1
        size: 7
      - id: type
        type: u2
        doc: Object file type (e.g., ET_EXEC, ET_DYN)
      - id: machine
        type: u2
        doc: Machine architecture (e.g., EM_ARM, EM_MIPS)
      - id: version2
        type: u4
        doc: Object file version
      - id: entry
        type: u4
        doc: Entry point address
      - id: phoff
        type: u4
        doc: Program header table file offset
      - id: shoff
        type: u4
        doc: Section header table file offset
      - id: flags
        type: u4
        doc: Processor-specific flags
      - id: ehsize
        type: u2
        doc: ELF header size
      - id: phentsize
        type: u2
        doc: Program header table entry size
      - id: phnum
        type: u2
        doc: Number of program header table entries
      - id: shentsize
        type: u2
        doc: Section header table entry size
      - id: shnum
        type: u2
        doc: Number of section header table entries
      - id: shstrndx
        type: u2
        doc: Section header string table index


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


  section_header:
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
    type: program_header
    repeat: expr
    repeat-expr: header.phnum
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: header.shnum
  - id: section_data
    type: u1
    repeat: expr
    repeat-expr: (lambda x: x.section_headers[-1].offset + x.section_headers[-1].size - (header.shoff + header.shentsize * header.shnum))

