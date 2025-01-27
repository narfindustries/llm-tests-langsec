# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethods(Enum):
        store = 0
        compress = 1
        pack = 2
        lzh = 3
        reserved_4 = 4
        reserved_5 = 5
        reserved_6 = 6
        reserved_7 = 7
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

    class FlagsEnum(Enum):
        text = 1
        header_crc = 2
        extra = 4
        name = 8
        comment = 16
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
        self.modification_time = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystems, self._io.read_u1())
        if (self.flags & Gzip.FlagsEnum.extra.value) != 0:
            self.extra_fields = Gzip.ExtraField(self._io, self, self._root)

        if (self.flags & Gzip.FlagsEnum.name.value) != 0:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

        if (self.flags & Gzip.FlagsEnum.comment.value) != 0:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

        if (self.flags & Gzip.FlagsEnum.header_crc.value) != 0:
            self.header_crc16 = self._io.read_u2le()

        if not (self._io.is_eof()):
            self.compressed_data = self._io.read_bytes_full()


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_subfields = self._io.read_u2le()
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



