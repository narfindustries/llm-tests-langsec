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
        self.id1 = self._io.read_u1()
        if not self.id1 == 31:
            raise kaitaistruct.ValidationNotEqualError(31, self.id1, self._io, u"/seq/0")
        self.id2 = self._io.read_u1()
        if not self.id2 == 139:
            raise kaitaistruct.ValidationNotEqualError(139, self.id2, self._io, u"/seq/1")
        self.cm = self._io.read_u1()
        if not self.cm == 8:
            raise kaitaistruct.ValidationNotEqualError(8, self.cm, self._io, u"/seq/2")
        self.flg = self._io.read_u1()
        self.mtime = self._io.read_u4le()
        self.xfl = self._io.read_u1()
        self.os = self._io.read_u1()
        if (self.flg & 4) != 0:
            self.extra = Gzip.ExtraFields(self._io, self, self._root)

        if (self.flg & 8) != 0:
            self.fname = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flg & 16) != 0:
            self.fcomment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flg & 2) != 0:
            self.hcrc = self._io.read_u2le()

        self.compressed_data = Gzip.CompressedData(self._io, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.isize = self._io.read_u4le()

    class ExtraFields(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.data = self._io.read_bytes(self.xlen)


    class CompressedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Strz(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"UTF-8")



