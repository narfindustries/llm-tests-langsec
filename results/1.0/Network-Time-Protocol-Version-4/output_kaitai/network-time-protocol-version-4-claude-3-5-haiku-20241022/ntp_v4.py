# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpV4(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = NtpV4.Header(self._io, self, self._root)
        if self.header.mode != 0:
            self.extension_fields = []
            i = 0
            while not self._io.is_eof():
                self.extension_fields.append(NtpV4.ExtensionField(self._io, self, self._root))
                i += 1



    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flags = self._io.read_u1()
            self.stratum = self._io.read_u1()
            self.poll = self._io.read_s1()
            self.precision = self._io.read_s1()
            self.root_delay = self._io.read_u4be()
            self.root_dispersion = self._io.read_u4be()
            self.reference_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.reference_timestamp = NtpV4.Timestamp(self._io, self, self._root)
            self.origin_timestamp = NtpV4.Timestamp(self._io, self, self._root)
            self.receive_timestamp = NtpV4.Timestamp(self._io, self, self._root)
            self.transmit_timestamp = NtpV4.Timestamp(self._io, self, self._root)

        @property
        def leap_indicator(self):
            """0: No warning
            1: Last minute has 61 seconds
            2: Last minute has 59 seconds
            3: Alarm (clock not synchronized)
            """
            if hasattr(self, '_m_leap_indicator'):
                return self._m_leap_indicator

            self._m_leap_indicator = ((self.flags & 192) >> 6)
            return getattr(self, '_m_leap_indicator', None)

        @property
        def version_number(self):
            """NTP version number."""
            if hasattr(self, '_m_version_number'):
                return self._m_version_number

            self._m_version_number = ((self.flags & 56) >> 3)
            return getattr(self, '_m_version_number', None)

        @property
        def mode(self):
            """0: Reserved
            1: Symmetric active
            2: Symmetric passive
            3: Client
            4: Server
            5: Broadcast
            6: NTP control message
            7: Private use
            """
            if hasattr(self, '_m_mode'):
                return self._m_mode

            self._m_mode = (self.flags & 7)
            return getattr(self, '_m_mode', None)


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
            self.field_length = self._io.read_u2be()
            self._raw_field_value = self._io.read_bytes(self.field_length)
            _io__raw_field_value = KaitaiStream(BytesIO(self._raw_field_value))
            self.field_value = NtpV4.ByteArray(_io__raw_field_value, self, self._root)


    class ByteArray(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = []
            for i in range(self._parent.field_length):
                self.data.append(self._io.read_u1())




