meta:
  id: elf
  file-extension: elf
  endian: le
  fields:
    - name: e_ident
      type: seq
      size: 16
      doc: ELF identification
      elements:
        - id: ei_mag
          type: seq
          size: 4
          doc: Magic number
        - id: ei_class
          type: u1
          doc: File class
        - id: ei_data
          type: u1
          doc: Data encoding
        - id: ei_version
          type: u1
          doc: File version
        - id: ei_osabi
          type: u1
          doc: OS/ABI identification
        - id: ei_abiversion
          type: u1
          doc: ABI version
        - id: ei_pad
          type: seq
          size: 7
          doc: Padding
    - name: e_type
      type: u2
      doc: Object file type
    - name: e_machine
      type: u2
      doc: Machine architecture
    - name: e_version
      type: u4
      doc: Object file version
    - name: e_entry
      type: u4
      doc: Entry point virtual address
    - name: e_phoff
      type: u4
      doc: Program header table file offset
    - name: e_shoff
      type: u4
      doc: Section header table file offset
    - name: e_flags
      type: u4
      doc: Processor-specific flags
    - name: e_ehsize
      type: u2
      doc: ELF header size
    - name: e_phentsize
      type: u2
      doc: Program header table entry size
    - name: e_phnum
      type: u2
      doc: Number of program header table entries
    - name: e_shentsize
      type: u2
      doc: Section header table entry size
    - name: e_shnum
      type: u2
      doc: Number of section header table entries
    - name: e_shstrndx
      type: u2
      doc: Section header string table index