# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gzip.Header(self._io, self, self._root)
        if self.header.flags.fextra:
            self.extra_fields = Gzip.ExtraFields(self._io, self, self._root)

        if self.header.flags.fname:
            self.filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.header.flags.fcomment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.header.flags.fhcrc:
            self.header_crc16 = self._io.read_u2le()

        self._raw_compressed_data = self._io.read_bytes_full()
        _io__raw_compressed_data = KaitaiStream(BytesIO(self._raw_compressed_data))
        self.compressed_data = Gzip.Deflate(_io__raw_compressed_data, self, self._root)
        self.trailer = Gzip.Trailer(self._io, self, self._root)

    class Deflate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ftext = self._io.read_bits_int_be(1) != 0
            self.fhcrc = self._io.read_bits_int_be(1) != 0
            self.fextra = self._io.read_bits_int_be(1) != 0
            self.fname = self._io.read_bits_int_be(1) != 0
            self.fcomment = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)


    class Strz(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")


    class Trailer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.isize = self._io.read_u4le()


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id1 = self._io.read_u1()
            if not self.id1 == 31:
                raise kaitaistruct.ValidationNotEqualError(31, self.id1, self._io, u"/types/header/seq/0")
            self.id2 = self._io.read_u1()
            if not self.id2 == 139:
                raise kaitaistruct.ValidationNotEqualError(139, self.id2, self._io, u"/types/header/seq/1")
            self.cm = self._io.read_u1()
            if not self.cm == 8:
                raise kaitaistruct.ValidationNotEqualError(8, self.cm, self._io, u"/types/header/seq/2")
            self.flags = Gzip.Flags(self._io, self, self._root)
            self.mtime = self._io.read_u4le()
            self.xfl = self._io.read_u1()
            self.os = self._io.read_u1()


    class ExtraFields(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.data = self._io.read_bytes(self.xlen)



