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
  - id: magic
    contents: [0x7F, 0x45, 0x4C, 0x46] # "\x7FELF"
  - id: bits
    type: u1
    enum: bits_type
    doc: "64 or 32 bit format."
  - id: endian
    type: u1
    enum: endian_type
    doc: "Endianness of the file."
  - id: header_version
    contents: [0x01]
    doc: "ELF header version."
  - id: os_abi
    type: u1
    enum: os_abi
    doc: "Operating system and ABI to which the object is targeted."
  - id: abi_version
    type: u1
    doc: "ABI version."
  - id: pad
    size: 7
    doc: "Padding bytes."
  - id: header
    type:
      switch-on: bits
      cases:
        bits_type::b32: elf_header_32
        bits_type::b64: elf_header_64

types:
  elf_header_32:
    seq:
      - id: type
        type: u2
        enum: type
      - id: machine
        type: u2
        enum: machine
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

  elf_header_64:
    seq:
      - id: type
        type: u2
        enum: type
      - id: machine
        type: u2
        enum: machine
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
  os_abi:
    0: system_v
    1: hpux
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
    17: sortix
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
    15: parisc
    17: vpp500
    18: sparc32plus
    19: i960
    20: ppc
    21: ppc64
    22: s390
    23: spu
    36: v800
    37: fr20
    38: rh32
    39: rce
    40: arm
    41: alpha
    42: sh
    43: sparc_v9
    44: tricore
    45: arc
    46: h8_300
    47: h8_300h
    48: h8s
    49: h8_500
    50: ia_64
    51: mips_x
    52: coldfire
    53: m68hc12
    54: mma
    55: pcp
    56: ncpu
    57: ndr1
    58: starc
    59: me16
    60: st100
    61: tinyj
    62: x86_64
    63: pdsp
    64: fx66
    66: st9plus
    67: st7
    68: m68hc16
    69: m68hc11
    70: m68hc08
    71: m68hc05
    72: s