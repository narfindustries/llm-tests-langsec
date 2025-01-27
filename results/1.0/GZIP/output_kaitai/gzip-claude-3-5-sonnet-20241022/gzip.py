# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        store = 0
        deflate = 8

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
        self.flags = Gzip.FlagsStruct(self._io, self, self._root)
        self.modification_time = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystems, self._io.read_u1())
        if not (self._io.is_eof()):
            self._raw_compressed_data = self._io.read_bytes_full()
            _io__raw_compressed_data = KaitaiStream(BytesIO(self._raw_compressed_data))
            self.compressed_data = Gzip.CompressedData(_io__raw_compressed_data, self, self._root)


    class FlagsStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text = self._io.read_bits_int_be(1) != 0
            self.header_crc = self._io.read_bits_int_be(1) != 0
            self.extra = self._io.read_bits_int_be(1) != 0
            self.name = self._io.read_bits_int_be(1) != 0
            self.comment = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)


    class CompressedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.flags.extra:
                self.extra_fields = []
                i = 0
                while True:
                    _ = Gzip.ExtraField(self._io, self, self._root)
                    self.extra_fields.append(_)
                    if _.id == 0:
                        break
                    i += 1

            if self._parent.flags.name:
                self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

            if self._parent.flags.comment:
                self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

            if self._parent.flags.header_crc:
                self.header_crc16 = self._io.read_u2le()

            self.deflate_data = self._io.read_bytes_full()


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u2le()
            self.len = self._io.read_u2le()
            self.data = self._io.read_bytes(self.len)



