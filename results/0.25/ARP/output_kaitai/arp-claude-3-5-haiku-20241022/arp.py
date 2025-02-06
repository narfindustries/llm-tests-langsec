# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareType(Enum):
        ethernet = 1
        ieee_802 = 6
        frame_relay = 15

    class ProtocolType(Enum):
        ipv4 = 2048
        arp = 2054
        ipv6 = 34525

    class OperationType(Enum):
        request = 1
        reply = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.htype = KaitaiStream.resolve_enum(Arp.HardwareType, self._io.read_u2be())
        self.ptype = KaitaiStream.resolve_enum(Arp.ProtocolType, self._io.read_u2be())
        self.hlen = self._io.read_u1()
        self.plen = self._io.read_u1()
        self.oper = KaitaiStream.resolve_enum(Arp.OperationType, self._io.read_u2be())
        self._raw_sender_hardware_addr = self._io.read_bytes(self.hlen)
        _io__raw_sender_hardware_addr = KaitaiStream(BytesIO(self._raw_sender_hardware_addr))
        self.sender_hardware_addr = Arp.HardwareAddress(_io__raw_sender_hardware_addr, self, self._root)
        self._raw_sender_protocol_addr = self._io.read_bytes(self.plen)
        _io__raw_sender_protocol_addr = KaitaiStream(BytesIO(self._raw_sender_protocol_addr))
        self.sender_protocol_addr = Arp.ProtocolAddress(_io__raw_sender_protocol_addr, self, self._root)
        self._raw_target_hardware_addr = self._io.read_bytes(self.hlen)
        _io__raw_target_hardware_addr = KaitaiStream(BytesIO(self._raw_target_hardware_addr))
        self.target_hardware_addr = Arp.HardwareAddress(_io__raw_target_hardware_addr, self, self._root)
        self._raw_target_protocol_addr = self._io.read_bytes(self.plen)
        _io__raw_target_protocol_addr = KaitaiStream(BytesIO(self._raw_target_protocol_addr))
        self.target_protocol_addr = Arp.ProtocolAddress(_io__raw_target_protocol_addr, self, self._root)

    class HardwareAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(self._parent.hlen):
                self.addr.append(self._io.read_u1())



    class ProtocolAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(self._parent.plen):
                self.addr.append(self._io.read_u1())




