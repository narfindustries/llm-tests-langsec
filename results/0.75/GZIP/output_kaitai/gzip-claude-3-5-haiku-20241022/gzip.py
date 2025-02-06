# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        deflate = 8

    class ExtraFlagsEnum(Enum):
        max_compression = 2
        fastest_compression = 4

    class OsEnum(Enum):
        fat = 0
        unix = 3
        macintosh = 7
        ntfs = 11
        unknown = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.identification = self._io.read_bytes(2)
        if not self.identification == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.identification, self._io, u"/seq/0")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
        self.flags = Gzip.FlagsType(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlagsEnum, self._io.read_u1())
        self.os = KaitaiStream.resolve_enum(Gzip.OsEnum, self._io.read_u1())
        if self.flags.fextra:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.fname:
            self.filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.fcomment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.fhcrc:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = Gzip.CompressedBlock(self._io, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()

    class FlagsType(KaitaiStruct):
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
            self.reserved1 = self._io.read_bits_int_be(1) != 0
            self.reserved2 = self._io.read_bits_int_be(1) != 0
            self.reserved3 = self._io.read_bits_int_be(1) != 0


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extra_len = self._io.read_u2le()
            self.extra_data = self._io.read_bytes(self.extra_len)


    class CompressedBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = Gzip.ByteArray(self._io, self, self._root)


    class ByteArray(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = []
            i = 0
            while not self._io.is_eof():
                self.value.append(self._io.read_u1())
                i += 1




