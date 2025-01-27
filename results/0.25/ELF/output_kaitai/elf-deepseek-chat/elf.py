# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        elf32 = 1
        elf64 = 2

    class ElfData(Enum):
        little_endian = 1
        big_endian = 2

    class ElfType(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
        core = 4

    class ElfOsAbi(Enum):
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
        fenix_os = 16
        cloudabi = 17
        stratus_openvos = 18

    class ElfMachine(Enum):
        none = 0
        m32 = 1
        sparc = 2
        i386 = 3
        m68k = 4
        m88k = 5
        i860 = 6
        mips = 7
        powerpc = 8
        s390 = 9
        arm = 10
        superh = 11
        ia_64 = 12
        x86_64 = 13
        aarch64 = 14
        riscv = 15
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
        self.version = self._io.read_u1()
        self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.padding = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2le())
        self.version_again = self._io.read_u4le()
        self.entry_point = self._io.read_u4le()
        self.phoff = self._io.read_u4le()
        self.shoff = self._io.read_u4le()
        self.flags = self._io.read_u4le()
        self.ehsize = self._io.read_u2le()
        self.phentsize = self._io.read_u2le()
        self.phnum = self._io.read_u2le()
        self.shentsize = self._io.read_u2le()
        self.shnum = self._io.read_u2le()
        self.shstrndx = self._io.read_u2le()

    class Elf32Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/elf32_header/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)
            self.type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2le())
            self.version_again = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.phoff = self._io.read_u4le()
            self.shoff = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.ehsize = self._io.read_u2le()
            self.phentsize = self._io.read_u2le()
            self.phnum = self._io.read_u2le()
            self.shentsize = self._io.read_u2le()
            self.shnum = self._io.read_u2le()
            self.shstrndx = self._io.read_u2le()


    class Elf64Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/elf64_header/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)
            self.type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2le())
            self.version_again = self._io.read_u4le()
            self.entry_point = self._io.read_u8le()
            self.phoff = self._io.read_u8le()
            self.shoff = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.ehsize = self._io.read_u2le()
            self.phentsize = self._io.read_u2le()
            self.phnum = self._io.read_u2le()
            self.shentsize = self._io.read_u2le()
            self.shnum = self._io.read_u2le()
            self.shstrndx = self._io.read_u2le()



