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
        self.li = self._io.read_bits_int_be(2)
        self.vn = self._io.read_bits_int_be(3)
        self.mode = self._io.read_bits_int_be(3)
        self._io.align_to_byte()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_u1()
        self.precision = self._io.read_s1()
        self.root_delay = NtpPacket.FixedPoint(self._io, self, self._root)
        self.root_dispersion = NtpPacket.FixedPoint(self._io, self, self._root)
        self.reference_id = self._io.read_u4be()
        self.ref_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.org_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.rec_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.xmt_timestamp = NtpPacket.NtpTimestamp(self._io, self, self._root)
        self.extensions = []
        i = 0
        while not self._io.is_eof():
            self.extensions.append(NtpPacket.Extension(self._io, self, self._root))
            i += 1


    class FixedPoint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.int_part = self._io.read_u2be()
            self.fraction_part = self._io.read_u2be()


    class NtpTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = self._io.read_u2be()
            self.extension_len = self._io.read_u2be()
            self.extension_data = []
            for i in range(self.extension_len):
                self.extension_data.append(self._io.read_u1())




