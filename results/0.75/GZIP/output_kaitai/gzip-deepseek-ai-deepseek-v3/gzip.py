# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        deflate = 8

    class OsTypes(Enum):
        fat = 0
        amiga = 1
        vms = 2
        unix = 3
        vm_cms = 4
        atari_tos = 5
        hpfs = 6
        macintosh = 7
        z_system = 8
        cp_m = 9
        tops_20 = 10
        ntfs = 11
        qdos = 12
        acorn_riscos = 13
        unknown = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gzip.GzipHeader(self._io, self, self._root)
        self.compressed_data = self._io.read_bytes_full()
        self.trailer = Gzip.GzipTrailer(self._io, self, self._root)

    class GzipHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id1 = self._io.read_u1()
            if not self.id1 == 31:
                raise kaitaistruct.ValidationNotEqualError(31, self.id1, self._io, u"/types/gzip_header/seq/0")
            self.id2 = self._io.read_u1()
            if not self.id2 == 139:
                raise kaitaistruct.ValidationNotEqualError(139, self.id2, self._io, u"/types/gzip_header/seq/1")
            self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
            self.flags = self._io.read_u1()
            self.modification_time = self._io.read_u4le()
            self.extra_flags = self._io.read_u1()
            self.os = KaitaiStream.resolve_enum(Gzip.OsTypes, self._io.read_u1())
            if (self.flags & 4) != 0:
                self.extra_field = Gzip.ExtraField(self._io, self, self._root)

            if (self.flags & 8) != 0:
                self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if (self.flags & 16) != 0:
                self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if (self.flags & 2) != 0:
                self.header_crc16 = self._io.read_u2le()



    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.data = self._io.read_bytes(self.xlen)


    class GzipTrailer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.isize = self._io.read_u4le()



