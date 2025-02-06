# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
    """Gzip is a file format used for file compression and decompression. The format
    is defined in RFC 1952. This specication is designed to be sufficient to
    decompress any Gzip file conforming to the RFC 1952 specification.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(2)
        if not self.magic == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/seq/0")
        self.compression_method = self._io.read_bytes(1)
        if not self.compression_method == b"\x08":
            raise kaitaistruct.ValidationNotEqualError(b"\x08", self.compression_method, self._io, u"/seq/1")
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mod_time = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.os = self._io.read_u1()
        if self.flags.has_extra:
            self.extras = Gzip.Extras(self._io, self, self._root)

        if self.flags.has_name:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.has_comment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.has_header_crc:
            self.header_crc16 = self._io.read_u2le()

        self._raw_body = self._io.read_bytes_full()
        _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
        self.body = Gzip.GzipBody(_io__raw_body, self, self._root)

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text = self._io.read_bits_int_be(1) != 0
            self.has_header_crc = self._io.read_bits_int_be(1) != 0
            self.has_extra = self._io.read_bits_int_be(1) != 0
            self.has_name = self._io.read_bits_int_be(1) != 0
            self.has_comment = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)


    class Extras(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_extra = self._io.read_u2le()
            self.extra_fields = self._io.read_bytes(self.len_extra)


    class GzipBody(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.compressed_data = self._io.read_bytes_full()
            self.crc32 = self._io.read_u4le()
            self.isize = self._io.read_u4le()



