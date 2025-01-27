# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Arp(KaitaiStruct):

    class ArpHardwareType(Enum):
        ethernet = 1
        ieee802 = 6
        arcnet = 7
        frame_relay = 15
        atm = 16
        hdlc = 17
        fibre_channel = 18
        atm2 = 19
        serial_line = 20

    class Ethertype(Enum):
        ipv4 = 2048
        arp = 2054
        rarp = 32821
        ipv6 = 33079

    class ArpOpcode(Enum):
        request = 1
        reply = 2
        request_reverse = 3
        reply_reverse = 4
        drarp_request = 5
        drarp_reply = 6
        drarp_error = 7
        inarp_request = 8
        inarp_reply = 9
        arp_nak = 10
        mars_request = 11
        mars_multi = 12
        mars_mserv = 13
        mars_join = 14
        mars_leave = 15
        mars_nak = 16
        mars_unspec = 17
        mars_mjoin = 18
        mars_mleave = 19
        mars_grouplist_request = 20
        mars_grouplist_reply = 21
        mars_redirect_map = 22
        mapos_arp = 23
        op_exp1 = 24
        op_exp2 = 25
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.hardware_type = KaitaiStream.resolve_enum(Arp.ArpHardwareType, self._io.read_u2be())
        self.protocol_type = KaitaiStream.resolve_enum(Arp.Ethertype, self._io.read_u2be())
        self.hardware_size = self._io.read_u1()
        self.protocol_size = self._io.read_u1()
        self.opcode = KaitaiStream.resolve_enum(Arp.ArpOpcode, self._io.read_u2be())
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




