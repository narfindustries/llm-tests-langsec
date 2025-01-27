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
        self.leap_indicator = self._io.read_bits_int_be(2)
        self.version = self._io.read_bits_int_be(3)
        self.mode = self._io.read_bits_int_be(3)
        self._io.align_to_byte()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_time = self._io.read_u8be()
        self.originate_time = self._io.read_u8be()
        self.receive_time = self._io.read_u8be()
        self.transmit_time = self._io.read_u8be()
        self.extension_fields = []
        i = 0
        while not self._io.is_eof():
            self.extension_fields.append(NtpV4.ExtensionField(self._io, self, self._root))
            i += 1

        self.authentication_data = NtpV4.AuthData(self._io, self, self._root)

    class ExtensionField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_type = self._io.read_u2be()
            self.field_length = self._io.read_u2be()
            self.field_value = self._io.read_bytes(self.field_length)
            self.padding = self._io.read_bytes(((4 - (self.field_length % 4)) % 4))


    class AuthData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key_id = self._io.read_u4be()
            self.message_digest = self._io.read_bytes(16)



