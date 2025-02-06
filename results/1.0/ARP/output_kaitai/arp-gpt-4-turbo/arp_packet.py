# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):

    class HardwareType(Enum):
        ethernet = 1
        ieee_802 = 6
        arcnet = 7
        frame_relay = 15

    class ProtocolType(Enum):
        ipv4 = 2048

    class Operation(Enum):
        request = 1
        reply = 2
        rarp_request = 3
        rarp_reply = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hrd = KaitaiStream.resolve_enum(ArpPacket.HardwareType, self._io.read_u2be())
        self.pro = KaitaiStream.resolve_enum(ArpPacket.ProtocolType, self._io.read_u2be())
        self.hln = self._io.read_u1()
        self.pln = self._io.read_u1()
        self.op = KaitaiStream.resolve_enum(ArpPacket.Operation, self._io.read_u2be())
        self.sha = self._io.read_bytes(self.hln)
        self.spa = self._io.read_bytes(self.pln)
        self.tha = self._io.read_bytes(self.hln)
        self.tpa = self._io.read_bytes(self.pln)


