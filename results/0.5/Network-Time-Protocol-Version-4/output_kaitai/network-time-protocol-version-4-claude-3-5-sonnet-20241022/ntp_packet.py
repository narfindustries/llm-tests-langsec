# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpPacket(KaitaiStruct):
    """Network Time Protocol Version 4 (NTPv4) packet format specification.
    RFC 5905: https://tools.ietf.org/html/rfc5905
    """

    class LeapIndicators(Enum):
        no_warning = 0
        last_minute_61 = 1
        last_minute_59 = 2
        alarm_condition = 3

    class Modes(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        ntp_control_message = 6
        reserved_private = 7
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.li_vn_mode = self._io.read_u1()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_s4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = NtpPacket.Timestamp64(self._io, self, self._root)
        self.origin_timestamp = NtpPacket.Timestamp64(self._io, self, self._root)
        self.receive_timestamp = NtpPacket.Timestamp64(self._io, self, self._root)
        self.transmit_timestamp = NtpPacket.Timestamp64(self._io, self, self._root)
        if self._io.is_eof() == False:
            self.extension_fields = []
            i = 0
            while True:
                _ = NtpPacket.ExtensionField(self._io, self, self._root)
                self.extension_fields.append(_)
                if self._io.is_eof() == True:
                    break
                i += 1


    class Timestamp64(KaitaiStruct):
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
            self.value = self._io.read_bytes((self.length - 4))


    @property
    def leap_indicator(self):
        """Leap Indicator (0-3)."""
        if hasattr(self, '_m_leap_indicator'):
            return self._m_leap_indicator

        self._m_leap_indicator = ((self.li_vn_mode & 192) >> 6)
        return getattr(self, '_m_leap_indicator', None)

    @property
    def version(self):
        """Version Number (4 for NTPv4)."""
        if hasattr(self, '_m_version'):
            return self._m_version

        self._m_version = ((self.li_vn_mode & 56) >> 3)
        return getattr(self, '_m_version', None)

    @property
    def mode(self):
        """Mode (0-7)."""
        if hasattr(self, '_m_mode'):
            return self._m_mode

        self._m_mode = (self.li_vn_mode & 7)
        return getattr(self, '_m_mode', None)


