# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        deflate = 8

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
        tops20 = 10
        ntfs = 11
        qdos = 12
        acorn = 13
        vfat = 14
        mvs = 15
        beos = 16
        tandem = 17
        theos = 18
        unknown = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gzip.Header(self._io, self, self._root)
        self.compressed_data = self._io.read_bytes_full()

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(2)
            if not self.magic == b"\x1F\x8B":
                raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/types/header/seq/0")
            self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethods, self._io.read_u1())
            self.flags = Gzip.Flags(self._io, self, self._root)
            self.modification_time = self._io.read_u4le()
            self.extra_flags = self._io.read_u1()
            self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystems, self._io.read_u1())
            if self.flags.fextra:
                self.optional_extra_fields = Gzip.OptionalExtraFields(self._io, self, self._root)

            if self.flags.fname:
                self.optional_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.flags.fcomment:
                self.optional_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.flags.fhcrc:
                self.optional_header_crc16 = self._io.read_u2le()



    class Flags(KaitaiStruct):
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
            self.reserved = self._io.read_bits_int_be(3)


    class OptionalExtraFields(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extra_length = self._io.read_u2le()
            self.extra_data = self._io.read_bytes(self.extra_length)


    class Footer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()



