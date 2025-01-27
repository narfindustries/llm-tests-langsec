# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):
    """The ARP (Address Resolution Protocol) is used for resolution of network layer addresses into link layer addresses, a critical function in the Internet protocol suite. ARP was defined in 1982 by RFC 826.
    """

    class HardwareType(Enum):
        ethernet = 1
        ieee_802 = 6
        fddi = 15

    class ProtocolType(Enum):
        ipv4 = 2048
        arp = 2054
        rarp = 32821
        ipv6 = 34525

    class Operation(Enum):
        request = 1
        reply = 2
        request_reverse = 3
        reply_reverse = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hw_type = KaitaiStream.resolve_enum(ArpPacket.HardwareType, self._io.read_u2be())
        self.proto_type = KaitaiStream.resolve_enum(ArpPacket.ProtocolType, self._io.read_u2be())
        self.hw_len = self._io.read_u1()
        self.proto_len = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(ArpPacket.Operation, self._io.read_u2be())
        self.sender_hw_addr = self._io.read_bytes(self.hw_len)
        self.sender_proto_addr = self._io.read_bytes(self.proto_len)
        self.target_hw_addr = self._io.read_bytes(self.hw_len)
        self.target_proto_addr = self._io.read_bytes(self.proto_len)


