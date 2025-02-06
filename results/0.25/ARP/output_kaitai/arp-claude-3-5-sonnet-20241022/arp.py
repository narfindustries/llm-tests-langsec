# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareTypeEnum(Enum):
        ethernet_10mb = 1
        ieee_802 = 6
        arcnet = 7
        frame_relay = 15
        atm = 16
        hdlc = 18
        fibre_channel = 19

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
        self.protocol_type = self._io.read_u2be()
        self.hardware_addr_len = self._io.read_u1()
        self.protocol_addr_len = self._io.read_u1()
        self.operation = KaitaiStream.resolve_enum(Arp.OperationEnum, self._io.read_u2be())
        self.sender_hardware_addr = self._io.read_bytes(self.hardware_addr_len)
        self.sender_protocol_addr = self._io.read_bytes(self.protocol_addr_len)
        self.target_hardware_addr = self._io.read_bytes(self.hardware_addr_len)
        self.target_protocol_addr = self._io.read_bytes(self.protocol_addr_len)

    @property
    def is_ethernet(self):
        if hasattr(self, '_m_is_ethernet'):
            return self._m_is_ethernet

        self._m_is_ethernet = self.hardware_type == Arp.HardwareTypeEnum.ethernet_10mb
        return getattr(self, '_m_is_ethernet', None)

    @property
    def is_ipv4(self):
        if hasattr(self, '_m_is_ipv4'):
            return self._m_is_ipv4

        self._m_is_ipv4 = self.protocol_type == 2048
        return getattr(self, '_m_is_ipv4', None)


