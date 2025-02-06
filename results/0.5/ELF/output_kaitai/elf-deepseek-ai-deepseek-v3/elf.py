# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        elfclassnone = 0
        elfclass32 = 1
        elfclass64 = 2

    class ElfData(Enum):
        elfdatnone = 0
        elfdata2lsb = 1
        elfdata2msb = 2

    class ElfOsabi(Enum):
        elfosabi_none = 0
        elfosabi_hpux = 1
        elfosabi_netbsd = 2
        elfosabi_linux = 3
        elfosabi_solaris = 6
        elfosabi_aix = 7
        elfosabi_irix = 8
        elfosabi_freebsd = 9
        elfosabi_tru64 = 10
        elfosabi_modesto = 11
        elfosabi_openbsd = 12
        elfosabi_arm_aeabi = 64
        elfosabi_arm = 97
        elfosabi_standalone = 255

    class ElfType(Enum):
        et_none = 0
        et_rel = 1
        et_exec = 2
        et_dyn = 3
        et_core = 4
        et_loos = 65024
        et_hios = 65279
        et_loproc = 65280
        et_hiproc = 65535

    class ElfVersion(Enum):
        ev_none = 0
        ev_current = 1

    class ElfMachine(Enum):
        em_none = 0
        em_m32 = 1
        em_sparc = 2
        em_386 = 3
        em_68k = 4
        em_88k = 5
        em_860 = 7
        em_mips = 8
        em_s370 = 9
        em_mips_rs3_le = 10
        em_parisc = 15
        em_vpp500 = 17
        em_sparc32plus = 18
        em_960 = 19
        em_ppc = 20
        em_ppc64 = 21
        em_s390 = 22
        em_spu = 23
        em_v800 = 36
        em_fr20 = 37
        em_rh32 = 38
        em_rce = 39
        em_arm = 40
        em_alpha = 41
        em_sh = 42
        em_sparcv9 = 43
        em_tricore = 44
        em_arc = 45
        em_h8_300 = 46
        em_h8_300h = 47
        em_h8s = 48
        em_h8_500 = 49
        em_ia_64 = 50
        em_mips_x = 51
        em_coldfire = 52
        em_68hc12 = 53
        em_mma = 54
        em_pcp = 55
        em_ncpu = 56
        em_ndr1 = 57
        em_starcore = 58
        em_me16 = 59
        em_st100 = 60
        em_tinyj = 61
        em_x86_64 = 62
        em_pdsp = 63
        em_pdp10 = 64
        em_pdp11 = 65
        em_fx66 = 66
        em_st9plus = 67
        em_st7 = 68
        em_68hc16 = 69
        em_68hc11 = 70
        em_68hc08 = 71
        em_68hc05 = 72
        em_svx = 73
        em_st19 = 74
        em_vax = 75
        em_cris = 76
        em_javelin = 77
        em_firepath = 78
        em_zsp = 79
        em_mmix = 80
        em_huany = 81
        em_prism = 82
        em_avr = 83
        em_fr30 = 84
        em_d10v = 85
        em_d30v = 86
        em_v850 = 87
        em_m32r = 88
        em_mn10300 = 89
        em_mn10200 = 90
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
        self.data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
        self.version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u1())
        self.osabi = KaitaiStream.resolve_enum(Elf.ElfOsabi, self._io.read_u1())
        self.abiversion = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2le())
        self.version_again = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u4le())
        if self.class == Elf.ElfClass.elfclass32:
            self.entry32 = self._io.read_u4le()

        if self.class == Elf.ElfClass.elfclass64:
            self.entry64 = self._io.read_u8le()

        if self.class == Elf.ElfClass.elfclass32:
            self.phoff32 = self._io.read_u4le()

        if self.class == Elf.ElfClass.elfclass64:
            self.phoff64 = self._io.read_u8le()

        if self.class == Elf.ElfClass.elfclass32:
            self.shoff32 = self._io.read_u4le()

        if self.class == Elf.ElfClass.elfclass64:
            self.shoff64 = self._io.read_u8le()

        self.flags = self._io.read_u4le()
        self.ehsize = self._io.read_u2le()
        self.phentsize = self._io.read_u2le()
        self.phnum = self._io.read_u2le()
        self.shentsize = self._io.read_u2le()
        self.shnum = self._io.read_u2le()
        self.shstrndx = self._io.read_u2le()


