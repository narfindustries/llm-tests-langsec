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
        cpm = 9
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
        self.magic = self._io.read_bytes(2)
        if not self.magic == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/seq/0")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
        self.flags = self._io.read_u1()
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.os = KaitaiStream.resolve_enum(Gzip.OsTypes, self._io.read_u1())
        if (self.flags & 4) != 0:
            self.extra_len = self._io.read_u2le()

        if (self.flags & 4) != 0:
            self._raw_extra = self._io.read_bytes(self.extra_len)
            _io__raw_extra = KaitaiStream(BytesIO(self._raw_extra))
            self.extra = Gzip.ExtraField(_io__raw_extra, self, self._root)

        if (self.flags & 8) != 0:
            self.original_file_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flags & 16) != 0:
            self.file_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flags & 2) != 0:
            self.header_crc16 = self._io.read_u2le()

        self._raw_compressed_data = self._io.read_bytes_full()
        _io__raw_compressed_data = KaitaiStream(BytesIO(self._raw_compressed_data))
        self.compressed_data = Gzip.CompressedBlock(_io__raw_compressed_data, self, self._root)

    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subfields = []
            i = 0
            while not self._io.is_eof():
                self.subfields.append(Gzip.Subfield(self._io, self, self._root))
                i += 1



    class Subfield(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u2le()
            self.len_data = self._io.read_u2le()
            self.data = self._io.read_bytes(self.len_data)


    class CompressedBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.blocks = []
            i = 0
            while not self._io.is_eof():
                self.blocks.append(self._io.read_u1())
                i += 1


        @property
        def crc32(self):
            if hasattr(self, '_m_crc32'):
                return self._m_crc32

            _pos = self._io.pos()
            self._io.seek(-8)
            self._m_crc32 = self._io.read_u4le()
            self._io.seek(_pos)
            return getattr(self, '_m_crc32', None)

        @property
        def isize(self):
            if hasattr(self, '_m_isize'):
                return self._m_isize

            _pos = self._io.pos()
            self._io.seek(-4)
            self._m_isize = self._io.read_u4le()
            self._io.seek(_pos)
            return getattr(self, '_m_isize', None)



