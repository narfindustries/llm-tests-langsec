# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpPacket(KaitaiStruct):

    class LeapIndicatorEnum(Enum):
        no_warning = 0
        last_minute_61_sec = 1
        last_minute_59_sec = 2
        alarm_condition = 3

    class ModeEnum(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        control = 6
        private = 7
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
        self.root_delay = NtpPacket.NtpShortFormat(self._io, self, self._root)
        self.root_dispersion = NtpPacket.NtpShortFormat(self._io, self, self._root)
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.origin_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.receive_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.transmit_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        if self._io.pos() < self._io.size():
            self.extension_fields = []
            i = 0
            while not self._io.is_eof():
                self.extension_fields.append(NtpPacket.ExtensionField(self._io, self, self._root))
                i += 1


        if self._io.pos() < self._io.size():
            self.mac = NtpPacket.MessageAuthCode(self._io, self, self._root)


    class NtpTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()


    class NtpShortFormat(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u2be()
            self.fraction = self._io.read_u2be()


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
            if (self._io.pos() % 4) != 0:
                self.padding = self._io.read_bytes(((4 - (self._io.pos() % 4)) % 4))



    class MessageAuthCode(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key_id = self._io.read_u4be()
            self.digest = self._io.read_bytes((self._io.size() - self._io.pos()))


    @property
    def leap_indicator(self):
        if hasattr(self, '_m_leap_indicator'):
            return self._m_leap_indicator

        self._m_leap_indicator = KaitaiStream.resolve_enum(NtpPacket.LeapIndicatorEnum, ((self.li_vn_mode >> 6) & 3))
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

        self._m_mode = KaitaiStream.resolve_enum(NtpPacket.ModeEnum, (self.li_vn_mode & 7))
        return getattr(self, '_m_mode', None)


