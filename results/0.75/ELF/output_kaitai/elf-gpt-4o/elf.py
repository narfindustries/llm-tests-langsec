# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        none = 0
        class32 = 1
        class64 = 2

    class ElfData(Enum):
        none = 0
        lsb = 1
        msb = 2
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
            self.e_ident = Elf.EIdent(self._io, self, self._root)
            self.e_type = self._io.read_u2be()
            self.e_machine = self._io.read_u2be()
            self.e_version = self._io.read_u4be()
            self.e_entry = self._io.read_u4be()
            self.e_phoff = self._io.read_u4be()
            self.e_shoff = self._io.read_u4be()
            self.e_flags = self._io.read_u4be()
            self.e_ehsize = self._io.read_u2be()
            self.e_phentsize = self._io.read_u2be()
            self.e_phnum = self._io.read_u2be()
            self.e_shentsize = self._io.read_u2be()
            self.e_shnum = self._io.read_u2be()
            self.e_shstrndx = self._io.read_u2be()


    class EIdent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/e_ident/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
            self.version = self._io.read_u1()
            self.osabi = self._io.read_u1()
            self.abiversion = self._io.read_u1()
            self.pad = self._io.read_bytes(7)



