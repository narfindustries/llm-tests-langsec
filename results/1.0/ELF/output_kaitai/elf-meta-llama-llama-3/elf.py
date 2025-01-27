# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self._raw_e_ident = self._io.read_bytes(16)
        _io__raw_e_ident = KaitaiStream(BytesIO(self._raw_e_ident))
        self.e_ident = Elf.ElfIdent(_io__raw_e_ident, self, self._root)
        self.e_type = self._io.read_u2le()
        self.e_machine = self._io.read_u2le()
        self.e_version = self._io.read_u4le()
        self.e_entry = self._io.read_u4le()
        self.e_phoff = self._io.read_u4le()
        self.e_shoff = self._io.read_u4le()
        self.e_flags = self._io.read_u4le()
        self.e_ehsize = self._io.read_u2le()
        self.e_phentsize = self._io.read_u2le()
        self.e_phnum = self._io.read_u2le()
        self.e_shentsize = self._io.read_u2le()
        self.e_shnum = self._io.read_u2le()
        self.e_shstrndx = self._io.read_u2le()

    class ElfIdent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = (self._io.read_bytes(4)).decode(u"ASCII")
            self.class = self._io.read_u1()
            self.data = self._io.read_u1()
            self.version = self._io.read_u1()
            self.osabi = self._io.read_u1()
            self.abiversion = self._io.read_u1()
            self.pad = (self._io.read_bytes(7)).decode(u"ASCII")



