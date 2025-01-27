# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ElfHeader(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_u4be()
        self.class = self._io.read_u1()
        self.data = self._io.read_u1()
        self.version = self._io.read_u1()
        self.osabi = self._io.read_u1()
        self.abiversion = self._io.read_u1()
        self.pad = self._io.read_u1()
        self.type = self._io.read_u2be()
        self.machine = self._io.read_u2be()
        self.version_elf = self._io.read_u4be()
        self.entry = self._io.read_u4be()
        self.phoff = self._io.read_u4be()
        self.shoff = self._io.read_u4be()
        self.flags = self._io.read_u4be()
        self.ehsize = self._io.read_u2be()
        self.phentsize = self._io.read_u2be()
        self.phnum = self._io.read_u2be()
        self.shentsize = self._io.read_u2be()
        self.shnum = self._io.read_u2be()
        self.shstrndx = self._io.read_u2be()


