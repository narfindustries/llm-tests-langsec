# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        deflate = 8

    class CompressionLevels(Enum):
        max_compression = 2
        fastest_algorithm = 4

    class OperatingSystems(Enum):
        fat = 0
        amiga = 1
        vms = 2
        unix = 3
        vm_cms = 4
        atari = 5
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
        self.identification = self._io.read_bytes(2)
        if not self.identification == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.identification, self._io, u"/seq/0")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
        self.flags = Gzip.FlagsType(self._io, self, self._root)
        self.modification_time = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.CompressionLevels, self._io.read_u1())
        self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystems, self._io.read_u1())
        if self.flags.extra_fields:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.original_filename:
            self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.file_comment:
            self.file_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.header_crc:
            self.header_crc = self._io.read_u2le()

        self.compressed_data = Gzip.DeflateBlock(self._io, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()

    class FlagsType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_hint = self._io.read_bits_int_be(1) != 0
            self.header_crc = self._io.read_bits_int_be(1) != 0
            self.extra_fields = self._io.read_bits_int_be(1) != 0
            self.original_filename = self._io.read_bits_int_be(1) != 0
            self.file_comment = self._io.read_bits_int_be(1) != 0
            self.encrypted = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(2)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2le()
            self.data = self._io.read_bytes(self.length)


    class DeflateBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.raw_data = (self._io.read_bytes_full()).decode(u"ASCII")



