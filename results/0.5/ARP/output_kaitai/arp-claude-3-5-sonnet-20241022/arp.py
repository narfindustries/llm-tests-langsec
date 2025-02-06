# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareTypes(Enum):
        ethernet_10mb = 1
        ieee_802 = 6
        arcnet = 7

    class ProtocolTypes(Enum):
        ipv4 = 2048

    class Operations(Enum):
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
        self.hardware_type = KaitaiStream.resolve_enum(Arp.HardwareTypes, self._io.read_u2be())
        self.protocol_type = KaitaiStream.resolve_enum(Arp.ProtocolTypes, self._io.read_u2be())
        self.hardware_addr_len = self._io.read_u1()
        self.protocol_addr_len = self._io.read_u1()
        self.operation = KaitaiStream.resolve_enum(Arp.Operations, self._io.read_u2be())
        self.sender_hardware_addr = self._io.read_bytes(self.hardware_addr_len)
        self.sender_protocol_addr = self._io.read_bytes(self.protocol_addr_len)
        self.target_hardware_addr = self._io.read_bytes(self.hardware_addr_len)
        self.target_protocol_addr = self._io.read_bytes(self.protocol_addr_len)


