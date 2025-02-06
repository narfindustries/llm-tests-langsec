meta:
  id: elf
  file-extension: elf
  endian: le
  license: CC0-1.0
doc: |
  The ELF file format is used for executables, shared libraries, and core dumps. This spec covers the ELF 64-bit structures.
seq:
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46]
  - id: bits
    type: u1
    enum: bits_type
  - id: endian
    type: u1
    enum: endian_type
  - id: header_version
    contents: [0x01]
  - id: os_abi
    type: u1
    enum: os_abi
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: type
    type: u2
    enum: obj_type
  - id: machine
    type: u2
    enum: machine_type
  - id: version
    type: u4
  - id: entry_point
    type: u8
  - id: ph_off
    type: u8
  - id: sh_off
    type: u8
  - id: flags
    type: u4
  - id: eh_size
    type: u2
  - id: ph_ent_size
    type: u2
  - id: ph_num
    type: u2
  - id: sh_ent_size
    type: u2
  - id: sh_num
    type: u2
  - id: sh_str_idx
    type: u2
enums:
  bits_type:
    1: b32
    2: b64
  endian_type:
    1: le
    2: be
  obj_type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
  machine_type:
    0: none
    3: x86
    62: x86_64
  os_abi:
    0: system_v
    1: hpux
    2: netbsd
    3: linux
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: modesto
    12: openbsd
    64: arm_aeabi
    97: arm
    255: standalone