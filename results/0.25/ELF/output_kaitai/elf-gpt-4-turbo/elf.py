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

    class Data(Enum):
        invalid = 0
        le = 1
        be = 2

    class OsAbi(Enum):
        system_v = 0
        hp_ux = 1
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
        openvos = 17

    class Version(Enum):
        invalid = 0
        current = 1

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

    class Class(Enum):
        invalid = 0
        class32 = 1
        class64 = 2

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
        self.header = Elf.Header(self._io, self, self._root)

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/header/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.Class, self._io.read_u1())
            self.data = KaitaiStream.resolve_enum(Elf.Data, self._io.read_u1())
            self.version = KaitaiStream.resolve_enum(Elf.Version, self._io.read_u1())
            self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.pad = self._io.read_bytes(7)
            self.type = KaitaiStream.resolve_enum(Elf.Type, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.e_version = self._io.read_u4le()
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



