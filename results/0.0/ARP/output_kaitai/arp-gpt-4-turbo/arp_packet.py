# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):
    """The Address Resolution Protocol (ARP) is a network protocol used to find out the address of a network node from its IP address.
    """

    class HardwareType(Enum):
        ethernet = 1
        ieee_802 = 6
        arcnet = 7
        frame_relay = 15
        atm = 16
        hdsl = 17
        fibre_channel = 18
        atm2 = 19
        serial_line = 20

    class ProtocolType(Enum):
        ipv4 = 2048
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
        self.hw_size = self._io.read_u1()
        self.proto_size = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(ArpPacket.Operation, self._io.read_u2be())
        self.src_hw_addr = self._io.read_bytes(self.src_hw_addr_size)
        self.src_proto_addr = self._io.read_bytes(self.src_proto_addr_size)
        self.dst_hw_addr = self._io.read_bytes(self.dst_hw_addr_size)
        self.dst_proto_addr = self._io.read_bytes(self.dst_proto_addr_size)

    @property
    def src_hw_addr_size(self):
        if hasattr(self, '_m_src_hw_addr_size'):
            return self._m_src_hw_addr_size

        self._m_src_hw_addr_size = self.hw_size
        return getattr(self, '_m_src_hw_addr_size', None)

    @property
    def src_proto_addr_size(self):
        if hasattr(self, '_m_src_proto_addr_size'):
            return self._m_src_proto_addr_size

        self._m_src_proto_addr_size = self.proto_size
        return getattr(self, '_m_src_proto_addr_size', None)

    @property
    def dst_hw_addr_size(self):
        if hasattr(self, '_m_dst_hw_addr_size'):
            return self._m_dst_hw_addr_size

        self._m_dst_hw_addr_size = self.hw_size
        return getattr(self, '_m_dst_hw_addr_size', None)

    @property
    def dst_proto_addr_size(self):
        if hasattr(self, '_m_dst_proto_addr_size'):
            return self._m_dst_proto_addr_size

        self._m_dst_proto_addr_size = self.proto_size
        return getattr(self, '_m_dst_proto_addr_size', None)


