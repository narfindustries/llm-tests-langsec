# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class Htype(Enum):
        ethernet = 1
        ieee802 = 6

    class Ptype(Enum):
        ipv4 = 2048

    class Oper(Enum):
        arp_request = 1
        arp_reply = 2
        rarp_request = 3
        rarp_reply = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.htype = self._io.read_u2be()
        self.ptype = self._io.read_u2be()
        self.hlen = self._io.read_u1()
        self.plen = self._io.read_u1()
        self.oper = self._io.read_u2be()
        self.sha = self._io.read_bytes(self.hlen)
        self.spa = self._io.read_bytes(self.plen)
        self.tha = self._io.read_bytes(self.hlen)
        self.tpa = self._io.read_bytes(self.plen)


