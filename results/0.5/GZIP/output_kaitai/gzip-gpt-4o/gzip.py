# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        deflate = 8

    class ExtraFlags(Enum):
        maximum_compression = 2
        fastest_compression = 4

    class Os(Enum):
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
        self.id1 = self._io.read_u1()
        if not self.id1 == 31:
            raise kaitaistruct.ValidationNotEqualError(31, self.id1, self._io, u"/seq/0")
        self.id2 = self._io.read_u1()
        if not self.id2 == 139:
            raise kaitaistruct.ValidationNotEqualError(139, self.id2, self._io, u"/seq/1")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
        self.flags = self._io.read_u1()
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.os = KaitaiStream.resolve_enum(Gzip.Os, self._io.read_u1())
        if (self.flags & 4) != 0:
            self.extra = Gzip.ExtraField(self._io, self, self._root)

        if (self.flags & 8) != 0:
            self.original_file_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if (self.flags & 16) != 0:
            self.file_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if (self.flags & 2) != 0:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = self._io.read_bytes_full()
        self.crc32 = self._io.read_u4le()
        self.isize = self._io.read_u4le()

    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subfield_id = self._io.read_u2le()
            self.len_subfield_data = self._io.read_u2le()
            self.subfield_data = self._io.read_bytes(self.len_subfield_data)



