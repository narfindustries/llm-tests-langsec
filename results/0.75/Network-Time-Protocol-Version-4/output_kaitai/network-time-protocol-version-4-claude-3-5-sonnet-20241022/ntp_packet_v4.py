# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpPacketV4(KaitaiStruct):

    class LeapIndicatorEnum(Enum):
        no_warning = 0
        last_minute_has_61_seconds = 1
        last_minute_has_59_seconds = 2
        alarm_condition = 3

    class ModeEnum(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        ntp_control_message = 6
        private = 7

    class StratumEnum(Enum):
        unspecified_or_invalid = 0
        primary_reference = 1
        secondary_reference = 2
        unsynchronized = 16
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.li_vn_mode = self._io.read_u1()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_u1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = NtpPacketV4.NtpTimestamp(self._io, self, self._root)
        self.origin_timestamp = NtpPacketV4.NtpTimestamp(self._io, self, self._root)
        self.receive_timestamp = NtpPacketV4.NtpTimestamp(self._io, self, self._root)
        self.transmit_timestamp = NtpPacketV4.NtpTimestamp(self._io, self, self._root)

    class NtpTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()


    @property
    def leap_indicator(self):
        if hasattr(self, '_m_leap_indicator'):
            return self._m_leap_indicator

        self._m_leap_indicator = ((self.li_vn_mode >> 6) & 3)
        return getattr(self, '_m_leap_indicator', None)

    @property
    def version(self):
        if hasattr(self, '_m_version'):
            return self._m_version

        self._m_version = ((self.li_vn_mode >> 3) & 7)
        return getattr(self, '_m_version', None)

    @property
    def mode(self):
        if hasattr(self, '_m_mode'):
            return self._m_mode

        self._m_mode = (self.li_vn_mode & 7)
        return getattr(self, '_m_mode', None)


