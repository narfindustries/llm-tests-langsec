meta:
  id: elf
  file-extension: elf
  endian: le
seq:
  - id: magic
    contents: [0x7f, 'E', 'L', 'F']
  - id: class
    type: u1
    enum: elf_class
  - id: data
    type: u1
    enum: elf_data
  - id: version
    type: u1
    enum: elf_version
  - id: osabi
    type: u1
    enum: elf_osabi
  - id: abiversion
    type: u1
  - id: pad
    size: 7
  - id: type
    type: u2
    enum: elf_type
  - id: machine
    type: u2
    enum: elf_machine
  - id: version_again
    type: u4
    enum: elf_version
  - id: entry32
    type: u4
    if: class == elf_class::elfclass32
  - id: entry64
    type: u8
    if: class == elf_class::elfclass64
  - id: phoff32
    type: u4
    if: class == elf_class::elfclass32
  - id: phoff64
    type: u8
    if: class == elf_class::elfclass64
  - id: shoff32
    type: u4
    if: class == elf_class::elfclass32
  - id: shoff64
    type: u8
    if: class == elf_class::elfclass64
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
    0: elfclassnone
    1: elfclass32
    2: elfclass64
  elf_data:
    0: elfdatnone
    1: elfdata2lsb
    2: elfdata2msb
  elf_version:
    0: ev_none
    1: ev_current
  elf_osabi:
    0: elfosabi_none
    1: elfosabi_hpux
    2: elfosabi_netbsd
    3: elfosabi_linux
    6: elfosabi_solaris
    7: elfosabi_aix
    8: elfosabi_irix
    9: elfosabi_freebsd
    10: elfosabi_tru64
    11: elfosabi_modesto
    12: elfosabi_openbsd
    64: elfosabi_arm_aeabi
    97: elfosabi_arm
    255: elfosabi_standalone
  elf_type:
    0: et_none
    1: et_rel
    2: et_exec
    3: et_dyn
    4: et_core
    0xfe00: et_loos
    0xfeff: et_hios
    0xff00: et_loproc
    0xffff: et_hiproc
  elf_machine:
    0: em_none
    1: em_m32
    2: em_sparc
    3: em_386
    4: em_68k
    5: em_88k
    7: em_860
    8: em_mips
    9: em_s370
    10: em_mips_rs3_le
    15: em_parisc
    17: em_vpp500
    18: em_sparc32plus
    19: em_960
    20: em_ppc
    21: em_ppc64
    22: em_s390
    23: em_spu
    36: em_v800
    37: em_fr20
    38: em_rh32
    39: em_rce
    40: em_arm
    41: em_alpha
    42: em_sh
    43: em_sparcv9
    44: em_tricore
    45: em_arc
    46: em_h8_300
    47: em_h8_300h
    48: em_h8s
    49: em_h8_500
    50: em_ia_64
    51: em_mips_x
    52: em_coldfire
    53: em_68hc12
    54: em_mma
    55: em_pcp
    56: em_ncpu
    57: em_ndr1
    58: em_starcore
    59: em_me16
    60: em_st100
    61: em_tinyj
    62: em_x86_64
    63: em_pdsp
    64: em_pdp10
    65: em_pdp11
    66: em_fx66
    67: em_st9plus
    68: em_st7
    69: em_68hc16
    70: em_68hc11
    71: em_68hc08
    72: em_68hc05
    73: em_svx
    74: em_st19
    75: em_vax
    76: em_cris
    77: em_javelin
    78: em_firepath
    79: em_zsp
    80: em_mmix
    81: em_huany
    82: em_prism
    83: em_avr
    84: em_fr30
    85: em_d10v
    86: em_d30v
    87: em_v850
    88: em_m32r
    89: em_mn10300
    90: em_mn10200