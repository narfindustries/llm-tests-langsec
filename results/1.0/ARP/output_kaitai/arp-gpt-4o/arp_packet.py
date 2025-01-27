# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):

    class ArpOperation(Enum):
        request = 1
        reply = 2
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
        self.operation = KaitaiStream.resolve_enum(ArpPacket.ArpOperation, self._io.read_u2be())
        self._raw_sender_hw_addr = self._io.read_bytes(self.hlen)
        _io__raw_sender_hw_addr = KaitaiStream(BytesIO(self._raw_sender_hw_addr))
        self.sender_hw_addr = ArpPacket.HwAddress(_io__raw_sender_hw_addr, self, self._root)
        self._raw_sender_proto_addr = self._io.read_bytes(self.plen)
        _io__raw_sender_proto_addr = KaitaiStream(BytesIO(self._raw_sender_proto_addr))
        self.sender_proto_addr = ArpPacket.Bignum(_io__raw_sender_proto_addr, self, self._root)
        self._raw_target_hw_addr = self._io.read_bytes(self.hlen)
        _io__raw_target_hw_addr = KaitaiStream(BytesIO(self._raw_target_hw_addr))
        self.target_hw_addr = ArpPacket.HwAddress(_io__raw_target_hw_addr, self, self._root)
        self._raw_target_proto_addr = self._io.read_bytes(self.plen)
        _io__raw_target_proto_addr = KaitaiStream(BytesIO(self._raw_target_proto_addr))
        self.target_proto_addr = ArpPacket.Bignum(_io__raw_target_proto_addr, self, self._root)

    class HwAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(self._parent.hlen):
                self.addr.append(self._io.read_u1())



    class Bignum(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number = []
            for i in range(self._parent.plen):
                self.number.append(self._io.read_u1())




