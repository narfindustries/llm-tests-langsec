# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class HardwareTypes(Enum):
        ethernet = 1

    class ProtocolTypes(Enum):
        ipv4 = 2048

    class Opcodes(Enum):
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
        self.hardware_size = self._io.read_u1()
        self.protocol_size = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(Arp.Opcodes, self._io.read_u2be())
        self._raw_sender_hardware_addr = self._io.read_bytes(self.hardware_size)
        _io__raw_sender_hardware_addr = KaitaiStream(BytesIO(self._raw_sender_hardware_addr))
        self.sender_hardware_addr = Arp.HardwareAddr(_io__raw_sender_hardware_addr, self, self._root)
        self._raw_sender_protocol_addr = self._io.read_bytes(self.protocol_size)
        _io__raw_sender_protocol_addr = KaitaiStream(BytesIO(self._raw_sender_protocol_addr))
        self.sender_protocol_addr = Arp.ProtocolAddr(_io__raw_sender_protocol_addr, self, self._root)
        self._raw_target_hardware_addr = self._io.read_bytes(self.hardware_size)
        _io__raw_target_hardware_addr = KaitaiStream(BytesIO(self._raw_target_hardware_addr))
        self.target_hardware_addr = Arp.HardwareAddr(_io__raw_target_hardware_addr, self, self._root)
        self._raw_target_protocol_addr = self._io.read_bytes(self.protocol_size)
        _io__raw_target_protocol_addr = KaitaiStream(BytesIO(self._raw_target_protocol_addr))
        self.target_protocol_addr = Arp.ProtocolAddr(_io__raw_target_protocol_addr, self, self._root)

    class HardwareAddr(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(self._root.hardware_size):
                self.addr.append(self._io.read_u1())



    class ProtocolAddr(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.addr = []
            for i in range(self._root.protocol_size):
                self.addr.append(self._io.read_u1())




