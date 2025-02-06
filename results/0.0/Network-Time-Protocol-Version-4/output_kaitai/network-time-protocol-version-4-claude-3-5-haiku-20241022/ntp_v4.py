# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpV4(KaitaiStruct):

    class LeapIndicatorType(Enum):
        no_warning = 0
        last_minute_61_seconds = 1
        last_minute_59_seconds = 2
        alarm_not_synchronized = 3

    class ModeType(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        ntp_control = 6
        private_use = 7
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = NtpV4.NtpHeader(self._io, self, self._root)

    class NtpHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flags = self._io.read_u1()
            self.stratum = self._io.read_u1()
            self.poll_interval = self._io.read_s1()
            self.precision = self._io.read_s1()
            self.root_delay = self._io.read_u4be()
            self.root_dispersion = self._io.read_u4be()
            self.reference_identifier = (self._io.read_bytes(4)).decode(u"ASCII")
            self.reference_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
            self.origin_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
            self.receive_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
            self.transmit_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)


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

        self._m_leap_indicator = KaitaiStream.resolve_enum(NtpV4.LeapIndicatorType, (self.header.flags >> 6))
        return getattr(self, '_m_leap_indicator', None)

    @property
    def version(self):
        if hasattr(self, '_m_version'):
            return self._m_version

        self._m_version = ((self.header.flags >> 3) & 7)
        return getattr(self, '_m_version', None)

    @property
    def mode(self):
        if hasattr(self, '_m_mode'):
            return self._m_mode

        self._m_mode = KaitaiStream.resolve_enum(NtpV4.ModeType, (self.header.flags & 7))
        return getattr(self, '_m_mode', None)


