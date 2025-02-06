# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class Compression(Enum):
        deflate = 8

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
        self.magic = self._io.read_bytes(2)
        if not self.magic == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/seq/0")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.Compression, self._io.read_u1())
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.operating_system = KaitaiStream.resolve_enum(Gzip.Os, self._io.read_u1())
        if self.flags.extra:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.name:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"iso-8859-1")

        if self.flags.comment:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"iso-8859-1")

        if self.flags.hcrc:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = self._io.read_bytes_full()
        self.footer = Gzip.Footer(self._io, self, self._root)

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_bits_int_be(3)
            self.comment = self._io.read_bits_int_be(1) != 0
            self.name = self._io.read_bits_int_be(1) != 0
            self.extra = self._io.read_bits_int_be(1) != 0
            self.hcrc = self._io.read_bits_int_be(1) != 0
            self.text = self._io.read_bits_int_be(1) != 0


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.extra_data = self._io.read_bytes(self.xlen)


    class Footer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.isize = self._io.read_u4le()



