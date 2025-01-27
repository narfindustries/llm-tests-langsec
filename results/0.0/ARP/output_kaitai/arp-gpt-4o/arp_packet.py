# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hardware_type = self._io.read_u2be()
        self.protocol_type = self._io.read_u2be()
        self.hardware_size = self._io.read_u1()
        self.protocol_size = self._io.read_u1()
        self.opcode = self._io.read_u2be()
        self.sender_mac = ArpPacket.MacAddress(self._io, self, self._root)
        self.sender_ip = ArpPacket.Ipv4Address(self._io, self, self._root)
        self.target_mac = ArpPacket.MacAddress(self._io, self, self._root)
        self.target_ip = ArpPacket.Ipv4Address(self._io, self, self._root)

    class MacAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.octets = []
            for i in range(6):
                self.octets.append(self._io.read_u1())



    class Ipv4Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.octets = []
            for i in range(4):
                self.octets.append(self._io.read_u1())




