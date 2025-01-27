meta:
  id: elf
  file-extension: elf
  endian: le
  license: CC0-1.0
  title: Executable and Linkable Format (ELF)
doc: |
  The Executable and Linkable Format (ELF) is a common standard file format for
  executables, object code, shared libraries, and core dumps. First published
  in the specification for the application binary interface (ABI) of the Unix
  operating system version named System V Release 4 (SVR4), and later in the
  Tool Interface Standard, it was quickly accepted among different vendors of
  Unix systems. In years since, it has become one of the most used formats for
  binary executables on Unix-like systems, and is also used on many other
  operating systems.

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic
        contents: [0x7F, 0x45, 0x4C, 0x46] # "\x7FELF"
      - id: class
        type: u1
        enum: class
      - id: data
        type: u1
        enum: data
      - id: version
        type: u1
        enum: version
      - id: os_abi
        type: u1
        enum: os_abi
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
      - id: e_version
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
  class:
    0: invalid
    1: class32
    2: class64

  data:
    0: invalid
    1: le
    2: be

  version:
    0: invalid
    1: current

  os_abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    4: gnu_hurd
    5: solaris
    6: aix
    7: irix
    8: freebsd
    9: tru64
    10: modesto
    11: openbsd
    12: openvms
    13: nsk
    14: aros
    15: fenixos
    16: cloudabi
    17: openvos

  type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
    0xFE00: lo_os
    0xFEFF: hi_os
    0xFF00: lo_proc
    0xFFFF: hi_proc

  machine:
    0: no_machine
    1: m32
    2: sparc
    3: x86
    4: m68k
    5: m88k
    6: iamcu
    7: i860
    8: mips
    9: s370
    10: mips_rs3_le
    # additional machine types could be added here