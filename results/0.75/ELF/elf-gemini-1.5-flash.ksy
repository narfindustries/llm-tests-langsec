# This is a placeholder.  The error message indicates a problem with the
# Kaitai Struct compiler and/or the input `.ksy` file, not the specification
# itself.  A correct specification requires the contents of `elf-gemini-1.5-flash.ksy`.
#  This example demonstrates a basic ELF header,  but will likely not work
# without modifications to match your specific ELF file structure.

meta:
  id: elf_header
  endian: be

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
  - id: type
    type: u2
  - id: machine
    type: u2
  - id: version_elf
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
