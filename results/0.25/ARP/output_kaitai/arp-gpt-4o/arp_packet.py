# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):

    class Operation(Enum):
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
        self.oper = KaitaiStream.resolve_enum(ArpPacket.Operation, self._io.read_u2be())
        self.sha = (self._io.read_bytes(self.hlen)).decode(u"ASCII")
        self.spa = (self._io.read_bytes(self.plen)).decode(u"ASCII")
        self.tha = (self._io.read_bytes(self.hlen)).decode(u"ASCII")
        self.tpa = (self._io.read_bytes(self.plen)).decode(u"ASCII")


