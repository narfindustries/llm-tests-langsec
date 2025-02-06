# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class Compression(Enum):
        store = 0
        deflate = 8

    class ExtraFlags(Enum):
        none = 0
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
        self.magic1 = self._io.read_bytes(1)
        if not self.magic1 == b"\x1F":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F", self.magic1, self._io, u"/seq/0")
        self.magic2 = self._io.read_bytes(1)
        if not self.magic2 == b"\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x8B", self.magic2, self._io, u"/seq/1")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.Compression, self._io.read_u1())
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.os = KaitaiStream.resolve_enum(Gzip.Os, self._io.read_u1())
        if self.flags.extra:
            self.extra_length = self._io.read_u2le()

        if self.flags.extra:
            self.extra = self._io.read_bytes(self.extra_length)

        if self.flags.name:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.comment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.header_crc:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = self._io.read_bytes(((self._io.size() - self._io.pos()) - 8))
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



