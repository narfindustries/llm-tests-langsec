meta:
  id: elf
  file-extension: elf
  endian: le
  doc: ELF (Executable and Linkable Format) is a common format for executables, object code, and shared libraries in Unix-like operating systems.
seq:
  - id: e_ident
    type: elf_header_e_ident
    doc: Identification bytes
  - id: e_type
    type: uint16
    doc: Object file type
  - id: e_machine
    type: uint16
    doc: Machine architecture
  - id: e_version
    type: uint32
    doc: Object file version
  - id: e_entry
    type: uint64
    doc: Virtual address to transfer control to
  - id: e_phoff
    type: uint64
    doc: Program header table file offset
  - id: e_shoff
    type: uint64
    doc: Section header table file offset
  - id: e_flags
    type: uint32
    doc: Processor-specific flags
  - id: e_ehsize
    type: uint16
    doc: ELF header size
  - id: e_phentsize
    type: uint16
    doc: Program header table entry size
  - id: e_phnum
    type: uint16
    doc: Number of program header table entries
  - id: e_shentsize
    type: uint16
    doc: Section header table entry size
  - id: e_shnum
    type: uint16
    doc: Number of section header table entries
  - id: e_shstrndx
    type: uint16
    doc: Section header table index of section name string table
types:
  elf_header_e_ident:
    seq:
      - id: magic
        type: str(4)
        doc: ELF magic number
      - id: class
        type: uint8
        doc: Class of the ELF file
      - id: data
        type: uint8
        doc: Data encoding of the ELF file
      - id: version
        type: uint8
        doc: ELF version
      - id: osabi
        type: uint8
        doc: Operating system/ABI identification
      - id: abiversion
        type: uint8
        doc: ABI version
      - id: pad
        type: str(7)
        doc: Padding bytes