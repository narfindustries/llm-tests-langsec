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
        arcnet = 7
        frame_relay = 15
        atm = 16
        hdlc = 17
        fibre_channel = 18
        atm_2 = 19
        serial_line = 20

    class Opcodes(Enum):
        request = 1
        reply = 2
        rarp_request = 3
        rarp_reply = 4
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
        self.hardware_type = KaitaiStream.resolve_enum(Arp.HardwareTypes, self._io.read_u2be())
        self.protocol_type = self._io.read_u2be()
        self.hardware_size = self._io.read_u1()
        self.protocol_size = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(Arp.Opcodes, self._io.read_u2be())
        self.sender_hardware_address = self._io.read_bytes(self.hardware_size)
        self.sender_protocol_address = self._io.read_bytes(self.protocol_size)
        self.target_hardware_address = self._io.read_bytes(self.hardware_size)
        self.target_protocol_address = self._io.read_bytes(self.protocol_size)


