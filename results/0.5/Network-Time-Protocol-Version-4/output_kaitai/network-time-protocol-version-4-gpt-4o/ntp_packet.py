# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpPacket(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.li_vn_mode = NtpPacket.LiVnMode(self._io, self, self._root)
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = self._io.read_u8be()
        self.originate_timestamp = self._io.read_u8be()
        self.receive_timestamp = self._io.read_u8be()
        self.transmit_timestamp = self._io.read_u8be()
        if (self._io.size() - self._io.pos()) > 0:
            self.optional_authenticator = NtpPacket.Authenticator(self._io, self, self._root)


    class LiVnMode(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.leap_indicator = self._io.read_bits_int_be(2)
            self.version_number = self._io.read_bits_int_be(3)
            self.mode = self._io.read_bits_int_be(3)


    class Authenticator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key_identifier = self._io.read_u4be()
            if (self._io.size() - self._io.pos()) >= 16:
                self.message_digest = self._io.read_bytes(16)




