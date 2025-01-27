meta:
  id: elf
  file-extension: elf
  endian: le
  title: Executable and Linkable Format (ELF)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The Executable and Linkable Format (ELF) is a common standard file format for
  executables, object code, shared libraries, and core dumps. First published
  in the specification for the application binary interface (ABI) of the Unix
  operating system version named System V Release 4 (SVR4), and later in the
  Tool Interface Standard, it was quickly accepted among different vendors of
  Unix systems. In years since, it has become one of the most used formats for
  binary executables in Unix and Unix-like systems on many hardware platforms.

seq:
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46]
  - id: bits
    type: u1
  - id: endian
    type: u1
  - id: ei_version
    type: u1
  - id: abi
    type: u1
  - id: abi_version
    type: u1
  - id: pad
    size: 7
  - id: header
    type:
      switch-on: bits
      cases:
        1: header32
        2: header64

types:
  header32:
    seq:
      - id: type
        type: u2
      - id: machine
        type: u2
      - id: version
        type: u4
      - id: entry_point
        type: u4
      - id: ph_off
        type: u4
      - id: sh_off
        type: u4
      - id: flags
        type: u4
      - id: eh_size
        type: u2
      - id: ph_entry_size
        type: u2
      - id: ph_num
        type: u2
      - id: sh_entry_size
        type: u2
      - id: sh_num
        type: u2
      - id: sh_str_idx
        type: u2

  header64:
    seq:
      - id: type
        type: u2
      - id: machine
        type: u2
      - id: version
        type: u8
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
      - id: ph_entry_size
        type: u2
      - id: ph_num
        type: u2
      - id: sh_entry_size
        type: u2
      - id: sh_num
        type: u2
      - id: sh_str_idx
        type: u2

enums:
  machine_type:
    0x02: sparc
    0x03: x86
    0x08: mips
    0x14: powerpc
    0x28: arm
    0x3E: x86_64
    0xB7: aarch64