# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareTypeEnum(Enum):
        ethernet = 1
        ieee_802 = 6
        framerelay = 15

    class ProtocolTypeEnum(Enum):
        ipv4 = 2048
        arp = 2054
        ipv6 = 34525

    class OperationEnum(Enum):
        request = 1
        reply = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hardware_type = KaitaiStream.resolve_enum(Arp.HardwareTypeEnum, self._io.read_u2be())
        self.protocol_type = KaitaiStream.resolve_enum(Arp.ProtocolTypeEnum, self._io.read_u2be())
        self.hardware_addr_length = self._io.read_u1()
        self.protocol_addr_length = self._io.read_u1()
        self.operation = KaitaiStream.resolve_enum(Arp.OperationEnum, self._io.read_u2be())
        self.sender_hardware_addr = (self._io.read_bytes(self.hardware_addr_length)).decode(u"ascii")
        self.sender_protocol_addr = (self._io.read_bytes(self.protocol_addr_length)).decode(u"ascii")
        self.target_hardware_addr = (self._io.read_bytes(self.hardware_addr_length)).decode(u"ascii")
        self.target_protocol_addr = (self._io.read_bytes(self.protocol_addr_length)).decode(u"ascii")


