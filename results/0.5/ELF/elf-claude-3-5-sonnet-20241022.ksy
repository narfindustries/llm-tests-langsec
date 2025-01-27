meta:
  id: elf
  file-extension: elf
  endian: le
  
seq:
  - id: magic
    contents: [0x7f, "ELF"]
  - id: bits
    type: u1
    enum: bits
  - id: endian
    type: u1
    enum: endian
  - id: version
    type: u1
  - id: abi
    type: u1
    enum: abi
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: type
    type: u2
    enum: type
  - id: machine
    type: u2
    enum: machine
  - id: version_2
    type: u4
  - id: entry_point
    type: u4
  - id: program_header_offset
    type: u4
  - id: section_header_offset
    type: u4
  - id: flags
    type: u4
  - id: header_size
    type: u2
  - id: program_header_entry_size
    type: u2
  - id: program_header_count
    type: u2
  - id: section_header_entry_size
    type: u2
  - id: section_header_count
    type: u2
  - id: section_names_idx
    type: u2

enums:
  bits:
    1: b32
    2: b64
    
  endian:
    1: le
    2: be
    
  type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core
    
  machine:
    0: none
    2: sparc
    3: x86
    8: mips
    0x14: powerpc
    0x28: arm
    0x3e: x86_64
    0xb7: aarch64
    
  abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    6: solaris
    7: aix
    8: irix
    9: freebsd
    0xa: tru64
    0xb: modesto
    0xc: openbsd
    0xd: openvms
    0xe: nsk
    0xf: aros
    0x11: fenixos
    0x12: cloudabi
    0x13: openvos