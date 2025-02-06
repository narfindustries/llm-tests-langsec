# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum
import zlib


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethod(Enum):
        reserved_0 = 0
        reserved_1 = 1
        reserved_2 = 2
        reserved_3 = 3
        reserved_4 = 4
        reserved_5 = 5
        reserved_6 = 6
        reserved_7 = 7
        deflate = 8
        reserved_9 = 9
        reserved_10 = 10
        reserved_11 = 11
        reserved_12 = 12
        reserved_13 = 13
        reserved_14 = 14
        reserved_15 = 15

    class ExtraFlags(Enum):
        maximum_compression = 2
        fastest_compression = 4

    class OperatingSystem(Enum):
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
        self.magic = self._io.read_bytes(2)
        if not self.magic == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/seq/0")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethod, self._io.read_u1())
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystem, self._io.read_u1())
        if self.flags.extra:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.name:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"iso-8859-1")

        if self.flags.comment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"iso-8859-1")

        if self.flags.header_crc:
            self.header_crc16 = self._io.read_u2le()

        self._raw_compressed_data = self._io.read_bytes(((self._io.size() - 8) - self._io.pos()))
        self.compressed_data = zlib.decompress(self._raw_compressed_data)
        self.crc32 = self._io.read_u4le()
        self.isize = self._io.read_u4le()

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_bits_int_le(3)
            self.comment = self._io.read_bits_int_le(1) != 0
            self.name = self._io.read_bits_int_le(1) != 0
            self.extra = self._io.read_bits_int_le(1) != 0
            self.header_crc = self._io.read_bits_int_le(1) != 0
            self.text = self._io.read_bits_int_le(1) != 0


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.extra_data = self._io.read_bytes(self.xlen)



