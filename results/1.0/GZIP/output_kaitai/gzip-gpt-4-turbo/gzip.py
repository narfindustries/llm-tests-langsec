# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
    """GZIP is a file format for file compression and decompression.
    GZIP format is a Lempel-Ziv coding (LZ77) with a 32-bit CRC.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gzip.Header(self._io, self, self._root)
        self.body = self._io.read_bytes_full()
        self.isize = self._io.read_u4le()

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(2)
            if not self.magic == b"\x1F\x8B":
                raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/types/header/seq/0")
            self.compression_method = self._io.read_u1()
            if not self.compression_method == 8:
                raise kaitaistruct.ValidationNotEqualError(8, self.compression_method, self._io, u"/types/header/seq/1")
            self.flags = Gzip.Header.Flags(self._io, self, self._root)
            self.mtime = self._io.read_u4le()
            self.extra_flags = self._io.read_u1()
            self.os = self._io.read_u1()
            if self.flags.has_extra:
                self.extras = Gzip.Header.Extras(self._io, self, self._root)

            if self.flags.has_name:
                self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.flags.has_comment:
                self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.flags.has_crc:
                self.hcrc = self._io.read_u2le()


        class Flags(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.has_extra = self._io.read_bits_int_be(1) != 0
                self.has_name = self._io.read_bits_int_be(1) != 0
                self.has_comment = self._io.read_bits_int_be(1) != 0
                self.has_crc = self._io.read_bits_int_be(1) != 0
                self.reserved = self._io.read_bits_int_be(4)


        class Extras(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.len = self._io.read_u2le()
                self.data = self._io.read_bytes(self.len)




