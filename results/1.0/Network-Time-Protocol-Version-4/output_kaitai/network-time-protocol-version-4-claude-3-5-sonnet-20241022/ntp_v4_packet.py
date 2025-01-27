# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpV4Packet(KaitaiStruct):
    """NTP version 4 packet format specification based on RFC 5905.
    Used for time synchronization between computer systems.
    """

    class Mode(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        control_message = 6
        private = 7

    class Leap(Enum):
        no_warning = 0
        last_minute_61 = 1
        last_minute_59 = 2
        alarm = 3
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = NtpV4Packet.HeaderFields(self._io, self, self._root)
        self.origin_timestamp = NtpV4Packet.Timestamp(self._io, self, self._root)
        self.receive_timestamp = NtpV4Packet.Timestamp(self._io, self, self._root)
        self.transmit_timestamp = NtpV4Packet.Timestamp(self._io, self, self._root)
        if self.header.extensions_present:
            self.extension_fields = []
            i = 0
            while True:
                _ = NtpV4Packet.ExtensionField(self._io, self, self._root)
                self.extension_fields.append(_)
                if _.end_of_extensions:
                    break
                i += 1


    class HeaderFields(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.leap_indicator = self._io.read_bits_int_be(2)
            self.version_number = self._io.read_bits_int_be(3)
            self.mode = self._io.read_bits_int_be(3)
            self._io.align_to_byte()
            self.stratum = self._io.read_u1()
            self.poll = self._io.read_s1()
            self.precision = self._io.read_s1()
            self.root_delay = self._io.read_u4be()
            self.root_dispersion = self._io.read_u4be()
            self.reference_id = self._io.read_u4be()
            self.reference_timestamp = NtpV4Packet.Timestamp(self._io, self, self._root)

        @property
        def extensions_present(self):
            """True if packet contains extension fields."""
            if hasattr(self, '_m_extensions_present'):
                return self._m_extensions_present

            self._m_extensions_present = self.mode == 7
            return getattr(self, '_m_extensions_present', None)


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
            self.type = self._io.read_u2be()
            self.length = self._io.read_u2be()
            if self.length > 4:
                self.value = self._io.read_bytes((self.length - 4))


        @property
        def end_of_extensions(self):
            """True if this is the last extension field."""
            if hasattr(self, '_m_end_of_extensions'):
                return self._m_end_of_extensions

            self._m_end_of_extensions = self.type == 0
            return getattr(self, '_m_end_of_extensions', None)



