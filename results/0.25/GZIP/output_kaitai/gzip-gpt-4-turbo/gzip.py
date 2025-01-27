# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
    """Gzip is a popular compression format used in various Internet applications.
    This specification describes the structure of a gzip file.
    """

    class CompressionMethods(Enum):
        deflate = 8

    class ExtraFlags(Enum):
        maximum = 2
        fast = 4
        super_fast = 6

    class OperatingSystems(Enum):
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
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mod_time = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.os = KaitaiStream.resolve_enum(Gzip.OperatingSystems, self._io.read_u1())
        if self.flags.has_extra:
            self.extras = Gzip.Extras(self._io, self, self._root)

        if self.flags.has_name:
            self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.has_comment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.has_crc:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = self._io.read_bytes_full()
        self.crc32 = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.has_text = self._io.read_bits_int_be(1) != 0
            self.has_crc = self._io.read_bits_int_be(1) != 0
            self.has_extra = self._io.read_bits_int_be(1) != 0
            self.has_name = self._io.read_bits_int_be(1) != 0
            self.has_comment = self._io.read_bits_int_be(1) != 0
            self.is_encrypted = self._io.read_bits_int_be(1) != 0
            self.is_reserved = self._io.read_bits_int_be(7)


    class Extras(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2le()
            self.data = self._io.read_bytes(self.len)



