# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

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
        self.id1 = self._io.read_u1()
        if not self.id1 == 31:
            raise kaitaistruct.ValidationNotEqualError(31, self.id1, self._io, u"/seq/0")
        self.id2 = self._io.read_u1()
        if not self.id2 == 139:
            raise kaitaistruct.ValidationNotEqualError(139, self.id2, self._io, u"/seq/1")
        self.cm = self._io.read_u1()
        if not self.cm == 8:
            raise kaitaistruct.ValidationNotEqualError(8, self.cm, self._io, u"/seq/2")
        self.flg = self._io.read_u1()
        self.mtime = self._io.read_u4le()
        self.xfl = self._io.read_u1()
        self.os = KaitaiStream.resolve_enum(Gzip.OsTypes, self._io.read_u1())
        if (self.flg & 4) != 0:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if (self.flg & 8) != 0:
            self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flg & 16) != 0:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flg & 2) != 0:
            self.header_crc16 = self._io.read_u2le()

        self._raw_compressed_data = self._io.read_bytes_full()
        _io__raw_compressed_data = KaitaiStream(BytesIO(self._raw_compressed_data))
        self.compressed_data = Gzip.Deflate(_io__raw_compressed_data, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.isize = self._io.read_u4le()

    class FixedHuffmanBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = (self._io.read_bytes_full()).decode(u"UTF-8")


    class Deflate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.blocks = []
            i = 0
            while not self._io.is_eof():
                self.blocks.append(Gzip.DeflateBlock(self._io, self, self._root))
                i += 1



    class DeflateBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bfinal = self._io.read_bits_int_be(1) != 0
            self.btype = self._io.read_bits_int_be(2)
            self._io.align_to_byte()
            _on = self.btype
            if _on == 0:
                self.data = Gzip.UncompressedBlock(self._io, self, self._root)
            elif _on == 1:
                self.data = Gzip.FixedHuffmanBlock(self._io, self, self._root)
            elif _on == 2:
                self.data = Gzip.DynamicHuffmanBlock(self._io, self, self._root)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.xlen = self._io.read_u2le()
            self.sub_fields = (self._io.read_bytes(self.xlen)).decode(u"UTF-8")


    class HuffmanCode(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.code_length = self._io.read_u1()


    class DynamicHuffmanBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.hlit = self._io.read_u1()
            self.hdist = self._io.read_u1()
            self.hclen = self._io.read_u1()
            self.code_lengths = []
            for i in range((self.hclen + 4)):
                self.code_lengths.append(self._io.read_u1())

            self.literal_length_codes = []
            for i in range((self.hlit + 257)):
                self.literal_length_codes.append(Gzip.HuffmanCode(self._io, self, self._root))

            self.distance_codes = []
            for i in range((self.hdist + 1)):
                self.distance_codes.append(Gzip.HuffmanCode(self._io, self, self._root))

            self.data = (self._io.read_bytes_full()).decode(u"UTF-8")


    class UncompressedBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2le()
            self.nlen = self._io.read_u2le()
            if not self.nlen == ~(self.len):
                raise kaitaistruct.ValidationNotEqualError(~(self.len), self.nlen, self._io, u"/types/uncompressed_block/seq/1")
            self.data = (self._io.read_bytes(self.len)).decode(u"UTF-8")



