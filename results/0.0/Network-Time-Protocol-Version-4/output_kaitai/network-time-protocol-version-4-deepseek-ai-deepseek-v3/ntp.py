# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Ntp(KaitaiStruct):

    class LeapIndicator(Enum):
        no_warning = 0
        last_minute_61_seconds = 1
        last_minute_59_seconds = 2
        alarm_condition = 3

    class VersionNumber(Enum):
        ntp_version_4 = 4

    class Mode(Enum):
        reserved = 0
        symmetric_active = 1
        symmetric_passive = 2
        client = 3
        server = 4
        broadcast = 5
        ntp_control_message = 6
        reserved_private_use = 7

    class Stratum(Enum):
        unspecified_or_invalid = 0
        primary_server = 1
        secondary_server_2 = 2
        secondary_server_3 = 3
        secondary_server_4 = 4
        secondary_server_5 = 5
        secondary_server_6 = 6
        secondary_server_7 = 7
        secondary_server_8 = 8
        secondary_server_9 = 9
        secondary_server_10 = 10
        secondary_server_11 = 11
        secondary_server_12 = 12
        secondary_server_13 = 13
        secondary_server_14 = 14
        secondary_server_15 = 15
        reserved_16 = 16
        reserved_255 = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.leap_indicator = KaitaiStream.resolve_enum(Ntp.LeapIndicator, self._io.read_bits_int_be(2))
        self.version_number = KaitaiStream.resolve_enum(Ntp.VersionNumber, self._io.read_bits_int_be(3))
        self.mode = KaitaiStream.resolve_enum(Ntp.Mode, self._io.read_bits_int_be(3))
        self._io.align_to_byte()
        self.stratum = KaitaiStream.resolve_enum(Ntp.Stratum, self._io.read_u1())
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = self._io.read_u8be()
        self.originate_timestamp = self._io.read_u8be()
        self.receive_timestamp = self._io.read_u8be()
        self.transmit_timestamp = self._io.read_u8be()
        if self.mode == Ntp.Mode.ntp_control_message:
            self.extension_fields = []
            i = 0
            while True:
                _ = self._io.read_u4be()
                self.extension_fields.append(_)
                if self._io.is_eof():
                    break
                i += 1

        if self.mode == Ntp.Mode.ntp_control_message:
            self.key_identifier = self._io.read_u4be()

        if self.mode == Ntp.Mode.ntp_control_message:
            self.mac = self._io.read_bytes(16)



