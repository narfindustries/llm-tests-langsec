meta:
  id: elf
  title: Executable and Linkable Format
  file-extension: elf
  application: executable file
  endian: le
  license: CC0-1.0
  ks-version: 0.9

seq:
  - id: magic
    contents: [0x7f, "ELF"]
  - id: bits
    type: u1
    enum: bits
  - id: endian
    type: u1
    enum: endian
  - id: ei_version
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
  - id: version
    type: u4
  - id: entry_point
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: program_header_offset
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: section_header_offset
    type:
      switch-on: bits
      cases:
        'bits::b32': u4
        'bits::b64': u8
  - id: flags
    type: u4
  - id: header_size
    type: u2
  - id: program_header_entry_size
    type: u2
  - id: program_header_num_entries
    type: u2
  - id: section_header_entry_size
    type: u2
  - id: section_header_num_entries
    type: u2
  - id: section_header_string_table_idx
    type: u2
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: program_header_num_entries
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: section_header_num_entries

types:
  program_header:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: flags
        type: u4
        if: _root.bits == bits::b64
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: vaddr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: paddr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: filesz
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: memsz
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: flags_1
        type: u4
        if: _root.bits == bits::b32
      - id: align
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8

  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: sh_type
      - id: flags
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: addr
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: offset
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: size
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: link
        type: u4
      - id: info
        type: u4
      - id: addr_align
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8
      - id: entry_size
        type:
          switch-on: _root.bits
          cases:
            'bits::b32': u4
            'bits::b64': u8

enums:
  bits:
    1: b32
    2: b64

  endian:
    1: le
    2: be

  type:
    0: none
    1: rel
    2: exec
    3: dyn
    4: core
    0xfe00: loos
    0xfeff: hios
    0xff00: loproc
    0xffff: hiproc

  machine:
    0: none
    1: m32
    2: sparc
    3: i386
    4: m68k
    5: m88k
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
    36: v800
    37: fr20
    38: rh32
    39: rce
    40: arm
    41: alpha
    42: sh
    43: sparcv9
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
    58: starcore
    59: me16
    60: st100
    61: tinyj
    62: x86_64
    63: pdsp
    66: fx66
    67: st9plus
    68: st7
    69: m68hc16
    70: m68hc11
    71: m68hc08
    72: m68hc05
    73: svx
    74: st19
    75: vax
    76: cris
    77: javelin
    78: firepath
    79: zsp
    80: mmix
    81: huany
    82: prism
    83: avr
    84: fr30
    85: d10v
    86: d30v
    87: v850
    88: m32r
    89: mn10300
    90: mn10200
    91: pj
    92: openrisc
    93: arc_a5
    94: xtensa
    95: videocore
    96: tmm_gpp
    97: ns32k
    98: tpc
    99: snp1k
    100: st200
    101: ip2k
    102: max
    103: cr
    104: f2mc16
    105: msp430
    106: blackfin
    107: se_c33
    108: sep3
    109: arca
    110: unicore
    111: excess
    112: dxp
    113: altera_nios2
    114: crx
    115: xgate
    116: c166
    117: m16c
    118: dspic30f
    119: ce
    120: m32c

  abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    4: gnu_hurd
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: novell_modesto
    12: openbsd
    13: openvms
    14: nonstop_kernel
    15: aros
    16: fenixos
    17: cloudabi
    18: openvos

  ph_type:
    0: pt_null
    1: pt_load
    2: pt_dynamic
    3: pt_interp
    4: pt_note
    5: pt_shlib
    6: pt_phdr
    7: pt_tls
    0x60000000: pt_loos
    0x6fffffff: pt_hios
    0x70000000: pt_loproc
    0x7fffffff: pt_hiproc

  sh_type:
    0: null_type
    1: progbits
    2: symtab
    3: strtab
    4: rela
    5: hash
    6: dynamic
    7: note
    8: nobits
    9: rel
    10: shlib
    11: dynsym
    14: init_array
    15: fini_array
    16: preinit_array
    17: group
    18: symtab_shndx
    0x60000000: loos
    0x6fffffff: hios
    0x70000000: loproc
    0x7fffffff: hiproc