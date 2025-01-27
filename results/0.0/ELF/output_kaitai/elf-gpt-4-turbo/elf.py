# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    """The Executable and Linkable Format (ELF) is a common standard file format for
    executables, object code, shared libraries, and core dumps. First published
    in the specification for the application binary interface (ABI) of the Unix
    operating system version named System V Release 4 (SVR4), and later in the
    Tool Interface Standard, it was quickly accepted among different vendors of
    Unix systems. In years since, it has become one of the most used formats for
    binary executables on Unix-like systems, and is also used on many other
    operating systems.
    """

    class EndianType(Enum):
        le = 1
        be = 2

    class BitsType(Enum):
        b32 = 1
        b64 = 2

    class OsAbi(Enum):
        system_v = 0
        hpux = 1
        netbsd = 2
        linux = 3
        gnu_hurd = 4
        solaris = 5
        aix = 6
        irix = 7
        freebsd = 8
        tru64 = 9
        modesto = 10
        openbsd = 11
        openvms = 12
        nsk = 13
        aros = 14
        fenixos = 15
        cloudabi = 16
        sortix = 17

    class Machine(Enum):
        no_machine = 0
        m32 = 1
        sparc = 2
        x86 = 3
        m68k = 4
        m88k = 5
        iamcu = 6
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
        spu = 23
        v800 = 36
        fr20 = 37
        rh32 = 38
        rce = 39
        arm = 40
        alpha = 41
        sh = 42
        sparc_v9 = 43
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
        starc = 58
        me16 = 59
        st100 = 60
        tinyj = 61
        x86_64 = 62
        pdsp = 63
        fx66 = 64
        st9plus = 66
        st7 = 67
        m68hc16 = 68
        m68hc11 = 69
        m68hc08 = 70
        m68hc05 = 71
        s = 72

    class Type(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
        core = 4
        lo_os = 65024
        hi_os = 65279
        lo_proc = 65280
        hi_proc = 65535
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = KaitaiStream.resolve_enum(Elf.BitsType, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.EndianType, self._io.read_u1())
        self.header_version = self._io.read_bytes(1)
        if not self.header_version == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.header_version, self._io, u"/seq/3")
        self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        _on = self.bits
        if _on == Elf.BitsType.b32:
            self.header = Elf.ElfHeader32(self._io, self, self._root)
        elif _on == Elf.BitsType.b64:
            self.header = Elf.ElfHeader64(self._io, self, self._root)

    class ElfHeader32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.Type, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.ph_off = self._io.read_u4le()
            self.sh_off = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.eh_size = self._io.read_u2le()
            self.ph_ent_size = self._io.read_u2le()
            self.ph_num = self._io.read_u2le()
            self.sh_ent_size = self._io.read_u2le()
            self.sh_num = self._io.read_u2le()
            self.sh_str_idx = self._io.read_u2le()


    class ElfHeader64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.Type, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u8le()
            self.entry_point = self._io.read_u8le()
            self.ph_off = self._io.read_u8le()
            self.sh_off = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.eh_size = self._io.read_u2le()
            self.ph_ent_size = self._io.read_u2le()
            self.ph_num = self._io.read_u2le()
            self.sh_ent_size = self._io.read_u2le()
            self.sh_num = self._io.read_u2le()
            self.sh_str_idx = self._io.read_u2le()



