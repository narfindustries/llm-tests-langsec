meta:
  id: elf_deepseek_chat
  file-extension: elf
  endian: le
  license: MIT
  ks-version: 0.9

seq:
  - id: magic
    contents: [0x7F, 'E', 'L', 'F']
  - id: class
    type: u1
    enum: elf_class
  - id: data
    type: u1
    enum: elf_data
  - id: version
    type: u1
  - id: os_abi
    type: u1
    enum: elf_os_abi
  - id: abi_version
    type: u1
  - id: padding
    size: 7
  - id: type
    type: u2
    enum: elf_type
  - id: machine
    type: u2
    enum: elf_machine
  - id: version_again
    type: u4
  - id: entry_point
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

enums:
  elf_class:
    1: elf_class_32
    2: elf_class_64

  elf_data:
    1: elf_data_2lsb
    2: elf_data_2msb

  elf_os_abi:
    0: elf_os_abi_system_v
    1: elf_os_abi_hp_ux
    2: elf_os_abi_netbsd
    3: elf_os_abi_linux
    4: elf_os_abi_gnu_hurd
    5: elf_os_abi_solaris
    6: elf_os_abi_aix
    7: elf_os_abi_irix
    8: elf_os_abi_freebsd
    9: elf_os_abi_tru64
    10: elf_os_abi_modesto
    11: elf_os_abi_openbsd
    12: elf_os_abi_openvms
    13: elf_os_abi_nsk
    14: elf_os_abi_aros
    15: elf_os_abi_fenixos
    16: elf_os_abi_cloudabi
    17: elf_os_abi_openvos

  elf_type:
    0: elf_type_none
    1: elf_type_rel
    2: elf_type_exec
    3: elf_type_dyn
    4: elf_type_core
    0xff00: elf_type_loos
    0xffff: elf_type_hios
    0xff01: elf_type_loproc
    0xfff1: elf_type_hiproc

  elf_machine:
    0: elf_machine_none
    1: elf_machine_m32
    2: elf_machine_sparc
    3: elf_machine_386
    4: elf_machine_68k
    5: elf_machine_88k
    6: elf_machine_860
    7: elf_machine_mips
    8: elf_machine_s370
    9: elf_machine_mips_rs3_le
    10: elf_machine_parisc
    11: elf_machine_vpp500
    12: elf_machine_sparc32plus
    13: elf_machine_960
    14: elf_machine_ppc
    15: elf_machine_ppc64
    16: elf_machine_s390
    17: elf_machine_v800
    18: elf_machine_fr20
    19: elf_machine_rh32
    20: elf_machine_rce
    21: elf_machine_arm
    22: elf_machine_alpha
    23: elf_machine_sh
    24: elf_machine_sparcv9
    25: elf_machine_tricore
    26: elf_machine_arc
    27: elf_machine_h8_300
    28: elf_machine_h8_300h
    29: elf_machine_h8s
    30: elf_machine_h8_500
    31: elf_machine_ia_64
    32: elf_machine_mips_x
    33: elf_machine_coldfire
    34: elf_machine_68hc12
    35: elf_machine_mma
    36: elf_machine_pcp
    37: elf_machine_ncpu
    38: elf_machine_ndr1
    39: elf_machine_starcore
    40: elf_machine_me16
    41: elf_machine_st100
    42: elf_machine_tinyj
    43: elf_machine_x86_64
    44: elf_machine_pdsp
    45: elf_machine_pdp10
    46: elf_machine_pdp11
    47: elf_machine_fx66
    48: elf_machine_st9plus
    49: elf_machine_st7
    50: elf_machine_68hc16
    51: elf_machine_68hc11
    52: elf_machine_68hc08
    53: elf_machine_68hc05
    54: elf_machine_svx
    55: elf_machine_st19
    56: elf_machine_vax
    57: elf_machine_cris
    58: elf_machine_javelin
    59: elf_machine_firepath
    60: elf_machine_zsp
    61: elf_machine_mmix
    62: elf_machine_human
    63: elf_machine_videocore
    64: elf_machine_tms320c6x
    65: elf_machine_microblaze
    66: elf_machine_cu
    67: elf_machine_ft32
    68: elf_machine_moxie
    69: elf_machine_amdil
    70: elf_machine_cloudshield
    71: elf_machine_corea
    72: elf_machine_arc_compact
    73: elf_machine_open8
    74: elf_machine_renesas_rx
    75: elf_machine_v850
    76: elf_machine_arcv2
    77: elf_machine_tms320c54x
    78: elf_machine_tms320c55x
    79: elf_machine_tms320c6x_c64plus
    80: elf_machine_tms320c6x_c64plus2
    81: elf_machine_tms320c6x_c64plus3
    82: elf_machine_tms320c6x_c64plus4
    83: elf_machine_tms320c6x_c64plus5
    84: elf_machine_tms320c6x_c64plus6
    85: elf_machine_tms320c6x_c64plus7
    86: elf_machine_tms320c6x_c64plus8
    87: elf_machine_tms320c6x_c64plus9
    88: elf_machine_tms320c6x_c64plus10
    89: elf_machine_tms320c6x_c64plus11
    90: elf_machine_tms320c6x_c64plus12
    91: elf_machine_tms320c6x_c64plus13
    92: elf_machine_tms320c6x_c64plus14
    93: elf_machine_tms320c6x_c64plus15
    94: elf_machine_tms320c6x_c64plus16
    95: elf_machine_tms320c6x_c64plus17
    96: elf_machine_tms320c6x_c64plus18
    97: elf_machine_tms320c6x_c64plus19
    98: elf_machine_tms320c6x_c64plus20
    99: elf_machine_tms320c6x_c64plus21
    100: elf_machine_tms320c6x_c64plus22
    101: elf_machine_tms320c6x_c64plus23
    102: elf_machine_tms320c6x_c64plus24
    103: elf_machine_tms320c6x_c64plus25
    104: elf_machine_tms320c6x_c64plus26
    105: elf_machine_tms320c6x_c64plus27
    106: elf_machine_tms320c6x_c64plus28
    107: elf_machine_tms320c6x_c64plus29
    108: elf_machine_tms320c6x_c64plus30
    109: elf_machine_tms320c6x_c64plus31
    110: elf_machine_tms320c6x_c64plus32
    111: elf_machine_tms320c6x_c64plus33
    112: elf_machine_tms320c6x_c64plus34
    113: elf_machine_tms320c6x_c64plus35
    114: elf_machine_tms320c6x_c64plus36
    115: elf_machine_tms320c6x_c64plus37
    116: elf_machine_tms320c6x_c64plus38
    117: elf_machine_tms320c6x_c64plus39
    118: elf_machine_tms320c6x_c64plus40
    119: elf_machine_tms320c6x_c64plus41
    120: elf_machine_tms320c6x_c64plus42
    121: elf_machine_tms320c6x_c64plus43
    122: elf_machine_tms320c6x_c64plus44
    123: elf_machine_tms320c6x_c64plus45
    124: elf_machine_tms320c6x_c64plus46
    125: elf_machine_tms320c6x_c64plus47
    126: elf_machine_tms320c6x_c64plus48
    127: elf_machine_tms320c6x_c64plus49
    128: elf_machine_tms320c6x_c64plus50
    129: elf_machine_tms320c6x_c64plus51
    130: elf_machine_tms320c6x_c64plus52
    131: elf_machine_tms320c6x_c64plus53
    132: elf_machine_tms320c6x_c64plus54
    133: elf_machine_tms320c6x_c64plus55
    134: elf_machine_tms320c6x_c64plus56
    135: elf_machine_tms320c6x_c64plus57
    136: elf_machine_tms320c6x_c64plus58
    137: elf_machine_tms320c6x_c64plus59
    138: elf_machine_tms320c6x_c64plus60
    139: elf_machine_tms320c6x_c64plus61
    140: elf_machine_tms320c6x_c64plus62
    141: elf_machine_tms320c6x_c64plus63
    142: elf_machine_tms320c6x_c64plus64
    143: elf_machine_tms320c6x_c64plus65
    144: elf_machine_tms320c6x_c64plus66
    145: elf_machine_tms320c6x_c64plus67
    146: elf_machine_tms320c6x_c64plus68
    147: elf_machine_tms320c6x_c64plus69
    148: elf_machine_tms320c6x_c64plus70
    149: elf_machine_tms320c6x_c64plus71
    150: elf_machine_tms320c6x_c64plus72
    151: elf_machine_tms320c6x_c64plus73
    152: elf_machine_tms320c6x_c64plus74
    153: elf_machine_tms320c6x_c64plus75
    154: elf_machine_tms320c6x_c64plus76
    155: elf_machine_tms320c6x_c64plus77
    156: elf_machine_tms320c6x_c64plus78
    157: elf_machine_tms320c6x_c64plus79
    158: elf_machine_tms320c6x_c64plus80
    159: elf_machine_tms320c6x_c64plus81
    160: elf_machine_tms320c6x_c64plus82
    161: elf_machine_tms320c6x_c64plus83
    162: elf_machine_tms320c6x_c64plus84
    163: elf_machine_tms320c6x_c64plus85
    164: elf_machine_tms320c6x_c64plus86
    165: elf_machine_tms320c6x_c64plus87
    166: elf_machine_tms320c6x_c64plus88
    167: elf_machine_tms320c6x_c64plus89
    168: elf_machine_tms320c6x_c64plus90
    169: elf_machine_tms320c6x_c64plus91
    170: elf_machine_tms320c6x_c64plus92
    171: elf_machine_tms320c6x_c64plus93
    172: elf_machine_tms320c6x_c64plus94
    173: elf_machine_tms320c6x_c64plus95
    174: elf_machine_tms320c6x_c64plus96
    175: elf_machine_tms320c6x_c64plus97
    176: elf_machine_tms320c6x_c64plus98
    177: elf_machine_tms320c6x_c64plus99
    178: elf_machine_tms320c6x_c64plus100
    179: elf_machine_tms320c6x_c64plus101
    180: elf_machine_tms320c6x_c64plus102
    181: elf_machine_tms320c6x_c64plus103
    182: elf_machine_tms320c6x_c64plus104
    183: elf_machine_tms320c6x_c64plus105
    184: elf_machine_tms320c6x_c64plus106
    185: elf_machine_tms320c6x_c64plus107
    186: elf_machine_tms320c6x_c64plus108
    187: elf_machine_tms320c6x_c64plus109
    188: elf_machine_tms320c6x_c64plus110
    189: elf_machine_tms320c6x_c64plus111
    190: elf_machine_tms320c6x_c64plus112
    191: elf_machine_tms320c6x_c64plus113
    192: elf_machine_tms320c6x_c64plus114
    193: elf_machine_tms320c6x_c64plus115
    194: elf_machine_tms320c6x_c64plus116
    195: elf_machine_tms320c6x_c64plus117
    196: elf_machine_tms320c6x_c64plus118
    197: elf_machine_tms320c6x_c64plus119
    198: elf_machine_tms320c6x_c64plus120
    199: elf_machine_tms320c6x_c64plus121
    200: elf_machine_tms320c6x_c64plus122
    201: elf_machine_tms320c6x_c64plus123
    202: elf_machine_tms320c6x_c64plus124
    203: elf_machine_tms320c6x_c64plus125
    204: elf_machine_tms320c6x_c64plus126
    205: elf_machine_tms320c6x_c64plus127
    206: elf_machine_tms320c6x_c64plus128
    207: elf_machine_tms320c6x_c64plus129
    208: elf_machine_tms320c6x_c64plus130
    209: elf_machine_tms320c6x_c64plus131
    210: elf_machine_tms320c6x_c64plus132
    211: elf_machine_tms320c6x_c64plus133
    212: elf_machine_tms320c6x_c64plus134
    213: elf_machine_tms320c6x_c64plus135
    214: elf_machine_tms320c6x_c64plus136
    215: elf_machine_tms320c6x_c64plus137
    216: elf_machine_tms320c6x_c64plus138
    217: elf_machine_tms320c6x_c64plus139
    218: elf_machine_tms320c6x_c64plus140
    219: elf_machine_tms320c6x_c64plus141
    220: elf_machine_tms320c6x_c64plus142
    221: elf_machine_tms320c6x_c64plus143
    222: elf_machine_tms320c6x_c64plus144
    223: elf_machine_tms320c6x_c64plus145
    224: elf_machine_tms320c6x_c64plus146
    225: elf_machine_tms320c6x_c64plus147
    226: elf_machine_tms320c6x_c64plus148
    227: elf_machine_tms320c6x_c64plus149
    228: elf_machine_tms320c6x_c64plus150
    229: elf_machine_tms320c6x_c64plus151
    230: elf_machine_tms320c6x_c64plus152
    231: elf_machine_tms320