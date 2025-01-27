# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NetworkTimeProtocolVersion4(KaitaiStruct):
    """The Network Time Protocol (NTP) is a protocol used to synchronize computer clock times in a network.
    """

    class LeapIndicator(Enum):
        no_warning = 0
        last_minute_61_seconds = 1
        last_minute_59_seconds = 2
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
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.ref_id = self._io.read_u4be()
        self.ref_timestamp = NetworkTimeProtocolVersion4.NtpTimestamp(self._io, self, self._root)
        self.orig_timestamp = NetworkTimeProtocolVersion4.NtpTimestamp(self._io, self, self._root)
        self.rx_timestamp = NetworkTimeProtocolVersion4.NtpTimestamp(self._io, self, self._root)
        self.tx_timestamp = NetworkTimeProtocolVersion4.NtpTimestamp(self._io, self, self._root)

    class NtpTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()



