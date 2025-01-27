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
    
  abi:
    0: system_v
    1: hp_ux
    2: netbsd
    3: linux
    4: hurd
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
    
  type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core
    
  machine:
    0: none
    1: m32
    2: sparc
    3: x86
    4: m68k
    5: m88k
    7: m860
    8: mips
    9: system370
    10: mips_rs3_le
    15: parisc
    17: vpp500
    18: sparc32plus
    19: m960
    20: powerpc
    21: powerpc64
    22: s390
    23: spu
    36: v800
    37: fr20
    38: rh32
    39: rce
    40: arm
    41: alpha
    42: superh
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
    64: pdp10
    65: pdp11
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
    93: arc_compact
    94: xtensa
    95: videocore
    96: tmm_gpp
    97: ns32k
    98: tpc
    99: snp1k
    100: st200
    101: ip2k
    102: max
    103: compact_risc
    104: f2mc16
    105: msp430
    106: blackfin
    107: se_c33
    108: sep3
    109: arc_compact2
    110: open8
    111: rl78
    112: videocore5
    113: 78kor
    114: 56800ex
    115: ba1
    116: ba2
    117: xcore
    118: mchp_pic
    119: km32
    120: kmx32
    121: kmx16
    122: kmx8
    123: kvarc
    124: cdp
    125: coge
    126: cool
    127: norc
    128: csr_kalimba
    129: z80
    130: visium
    131: ft32
    132: moxie
    133: amdgpu
    134: riscv
    135: bpf
    136: csky
    137: loongarch