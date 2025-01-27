# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareTypes(Enum):
        ethernet = 1
        ieee_802 = 6
        frame_relay = 15
        atm = 16
        hdlc = 17
        fibre_channel = 18

    class ProtocolTypes(Enum):
        ipv4 = 2048
        arp = 2054
        ipv6 = 34525

    class ArpOperation(Enum):
        request = 1
        reply = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hardware_type = KaitaiStream.resolve_enum(Arp.HardwareTypes, self._io.read_u2be())
        self.protocol_type = KaitaiStream.resolve_enum(Arp.ProtocolTypes, self._io.read_u2be())
        self.hardware_address_length = self._io.read_u1()
        self.protocol_address_length = self._io.read_u1()
        self.operation = KaitaiStream.resolve_enum(Arp.ArpOperation, self._io.read_u2be())
        self._raw_sender_hardware_address = self._io.read_bytes(self.hardware_address_length)
        _io__raw_sender_hardware_address = KaitaiStream(BytesIO(self._raw_sender_hardware_address))
        self.sender_hardware_address = Arp.HardwareAddress(_io__raw_sender_hardware_address, self, self._root)
        self._raw_sender_protocol_address = self._io.read_bytes(self.protocol_address_length)
        _io__raw_sender_protocol_address = KaitaiStream(BytesIO(self._raw_sender_protocol_address))
        self.sender_protocol_address = Arp.ProtocolAddress(_io__raw_sender_protocol_address, self, self._root)
        self._raw_target_hardware_address = self._io.read_bytes(self.hardware_address_length)
        _io__raw_target_hardware_address = KaitaiStream(BytesIO(self._raw_target_hardware_address))
        self.target_hardware_address = Arp.HardwareAddress(_io__raw_target_hardware_address, self, self._root)
        self._raw_target_protocol_address = self._io.read_bytes(self.protocol_address_length)
        _io__raw_target_protocol_address = KaitaiStream(BytesIO(self._raw_target_protocol_address))
        self.target_protocol_address = Arp.ProtocolAddress(_io__raw_target_protocol_address, self, self._root)

    class HardwareAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(self._parent.hardware_address_length):
                self.address.append(self._io.read_u1())



    class ProtocolAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(self._parent.protocol_address_length):
                self.address.append(self._io.read_u1())




