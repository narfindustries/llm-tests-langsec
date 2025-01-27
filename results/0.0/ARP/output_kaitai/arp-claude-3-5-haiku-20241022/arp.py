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
        frame_relay = 15
        atm = 16
        hdlc = 17
        fibre_channel = 18

    class ProtocolTypeEnum(Enum):
        ipv4 = 2048
        arp = 2054
        ipv6 = 34525

    class OpcodeEnum(Enum):
        request = 1
        reply = 2
        request_reverse = 3
        reply_reverse = 4
        drarp_request = 5
        drarp_reply = 6
        drarp_error = 7
        inarp_request = 8
        inarp_reply = 9
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hardware_type = KaitaiStream.resolve_enum(Arp.HardwareTypeEnum, self._io.read_u2be())
        self.protocol_type = KaitaiStream.resolve_enum(Arp.ProtocolTypeEnum, self._io.read_u2be())
        self.hardware_size = self._io.read_u1()
        self.protocol_size = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(Arp.OpcodeEnum, self._io.read_u2be())
        self.sender_mac_addr = Arp.MacAddress(self._io, self, self._root)
        self.sender_ip_addr = Arp.Ipv4Address(self._io, self, self._root)
        self.target_mac_addr = Arp.MacAddress(self._io, self, self._root)
        self.target_ip_addr = Arp.Ipv4Address(self._io, self, self._root)

    class MacAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(6):
                self.addr.append(self._io.read_u1())



    class Ipv4Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(4):
                self.addr.append(self._io.read_u1())




