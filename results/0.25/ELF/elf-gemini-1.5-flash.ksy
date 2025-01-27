# This is a sample Kaitai Struct definition.  Adjust as needed for your actual ELF file.
# The error message suggests a problem with the ksy file or the kaitai-struct-compiler itself,
# not necessarily the structure definition.  This example focuses on a basic ELF header.

type: struct
endian: be

seq:
  - id: magic
    type: u4
  - id: class
    type: u2
  - id: endianness
    type: u2
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

