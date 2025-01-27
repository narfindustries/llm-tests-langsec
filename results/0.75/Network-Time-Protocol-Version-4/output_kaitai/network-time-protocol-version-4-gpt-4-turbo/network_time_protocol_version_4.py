# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NetworkTimeProtocolVersion4(KaitaiStruct):
    """Specification for the Network Time Protocol (NTP), which is used to synchronize
    the time on computers over a network. This spec covers version 4 of the protocol.
    """

    class LeapIndicator(Enum):
        no_warning = 0
        last_minute_has_61_seconds = 1
        last_minute_has_59_seconds = 2
        alarm_condition = 3

    class Mode(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        reserved_for_ntp_control_message = 6
        reserved_for_private_use = 7
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.leap_indicator = KaitaiStream.resolve_enum(NetworkTimeProtocolVersion4.LeapIndicator, self._io.read_bits_int_be(2))
        self.version = self._io.read_bits_int_be(3)
        self.mode = KaitaiStream.resolve_enum(NetworkTimeProtocolVersion4.Mode, self._io.read_bits_int_be(3))
        self._io.align_to_byte()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_u1()
        self.precision = self._io.read_s1()
        self.root_delay = NetworkTimeProtocolVersion4.NtpShort(self._io, self, self._root)
        self.root_dispersion = NetworkTimeProtocolVersion4.NtpShort(self._io, self, self._root)
        self.reference_id = self._io.read_bytes(4)
        self.reference_timestamp = NetworkTimeProtocolVersion4.Timestamp(self._io, self, self._root)
        self.originate_timestamp = NetworkTimeProtocolVersion4.Timestamp(self._io, self, self._root)
        self.receive_timestamp = NetworkTimeProtocolVersion4.Timestamp(self._io, self, self._root)
        self.transmit_timestamp = NetworkTimeProtocolVersion4.Timestamp(self._io, self, self._root)
        self.extension = []
        i = 0
        while not self._io.is_eof():
            self.extension.append(NetworkTimeProtocolVersion4.ExtensionField(self._io, self, self._root))
            i += 1


    class NtpShort(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_s2be()
            self.fraction = self._io.read_u2be()


    class Timestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()


    class ExtensionField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_type = self._io.read_u2be()
            self.length = self._io.read_u2be()
            self.data = self._io.read_bytes(self.length)



