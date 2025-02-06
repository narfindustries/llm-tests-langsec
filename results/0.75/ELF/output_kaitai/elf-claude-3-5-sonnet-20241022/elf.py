# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class Endian(Enum):
        le = 1
        be = 2

    class ShType(Enum):
        null_type = 0
        progbits = 1
        symtab = 2
        strtab = 3
        rela = 4
        hash = 5
        dynamic = 6
        note = 7
        nobits = 8
        rel = 9
        shlib = 10
        dynsym = 11
        init_array = 14
        fini_array = 15
        preinit_array = 16
        group = 17
        symtab_shndx = 18
        loos = 1610612736
        hios = 1879048191
        loproc = 1879048192
        hiproc = 2147483647

    class Abi(Enum):
        system_v = 0
        hp_ux = 1
        netbsd = 2
        linux = 3
        gnu_hurd = 4
        solaris = 6
        aix = 7
        irix = 8
        freebsd = 9
        tru64 = 10
        novell_modesto = 11
        openbsd = 12
        openvms = 13
        nonstop_kernel = 14
        aros = 15
        fenixos = 16
        cloudabi = 17
        openvos = 18

    class Machine(Enum):
        none = 0
        m32 = 1
        sparc = 2
        i386 = 3
        m68k = 4
        m88k = 5
        i860 = 7
        mips = 8
        s370 = 9
        mips_rs3_le = 10
        parisc = 15
        vpp500 = 17
        sparc32plus = 18
        i960 = 19
        ppc = 20
        ppc64 = 21
        s390 = 22
        v800 = 36
        fr20 = 37
        rh32 = 38
        rce = 39
        arm = 40
        alpha = 41
        sh = 42
        sparcv9 = 43
        tricore = 44
        arc = 45
        h8_300 = 46
        h8_300h = 47
        h8s = 48
        h8_500 = 49
        ia_64 = 50
        mips_x = 51
        coldfire = 52
        m68hc12 = 53
        mma = 54
        pcp = 55
        ncpu = 56
        ndr1 = 57
        starcore = 58
        me16 = 59
        st100 = 60
        tinyj = 61
        x86_64 = 62
        pdsp = 63
        fx66 = 66
        st9plus = 67
        st7 = 68
        m68hc16 = 69
        m68hc11 = 70
        m68hc08 = 71
        m68hc05 = 72
        svx = 73
        st19 = 74
        vax = 75
        cris = 76
        javelin = 77
        firepath = 78
        zsp = 79
        mmix = 80
        huany = 81
        prism = 82
        avr = 83
        fr30 = 84
        d10v = 85
        d30v = 86
        v850 = 87
        m32r = 88
        mn10300 = 89
        mn10200 = 90
        pj = 91
        openrisc = 92
        arc_a5 = 93
        xtensa = 94
        videocore = 95
        tmm_gpp = 96
        ns32k = 97
        tpc = 98
        snp1k = 99
        st200 = 100
        ip2k = 101
        max = 102
        cr = 103
        f2mc16 = 104
        msp430 = 105
        blackfin = 106
        se_c33 = 107
        sep3 = 108
        arca = 109
        unicore = 110
        excess = 111
        dxp = 112
        altera_nios2 = 113
        crx = 114
        xgate = 115
        c166 = 116
        m16c = 117
        dspic30f = 118
        ce = 119
        m32c = 120

    class Bits(Enum):
        b32 = 1
        b64 = 2

    class Type(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
        core = 4
        loos = 65024
        hios = 65279
        loproc = 65280
        hiproc = 65535

    class PhType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6
        pt_tls = 7
        pt_loos = 1610612736
        pt_hios = 1879048191
        pt_loproc = 1879048192
        pt_hiproc = 2147483647
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = KaitaiStream.resolve_enum(Elf.Bits, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.Endian, self._io.read_u1())
        self.ei_version = self._io.read_u1()
        self.abi = KaitaiStream.resolve_enum(Elf.Abi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.Type, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.version = self._io.read_u4le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.entry_point = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.entry_point = self._io.read_u8le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.program_header_offset = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.program_header_offset = self._io.read_u8le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.section_header_offset = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.section_header_offset = self._io.read_u8le()
        self.flags = self._io.read_u4le()
        self.header_size = self._io.read_u2le()
        self.program_header_entry_size = self._io.read_u2le()
        self.program_header_num_entries = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.section_header_num_entries = self._io.read_u2le()
        self.section_header_string_table_idx = self._io.read_u2le()
        self.program_headers = []
        for i in range(self.program_header_num_entries):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

        self.section_headers = []
        for i in range(self.section_header_num_entries):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root))


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            if self._root.bits == Elf.Bits.b64:
                self.flags = self._io.read_u4le()

            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.vaddr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.vaddr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.paddr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.paddr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.filesz = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.filesz = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.memsz = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.memsz = self._io.read_u8le()
            if self._root.bits == Elf.Bits.b32:
                self.flags_1 = self._io.read_u4le()

            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.align = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.align = self._io.read_u8le()


    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = KaitaiStream.resolve_enum(Elf.ShType, self._io.read_u4le())
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.flags = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.flags = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.addr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.addr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.size = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.size = self._io.read_u8le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.addr_align = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.addr_align = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.entry_size = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.entry_size = self._io.read_u8le()



