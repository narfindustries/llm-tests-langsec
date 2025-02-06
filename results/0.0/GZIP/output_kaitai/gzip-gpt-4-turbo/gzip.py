# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
    """Gzip is a file format used for file compression and decompression. The format was introduced by the GNU project in 1992, and is based on the DEFLATE algorithm, which is a combination of LZ77 and Huffman coding.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.members = []
        i = 0
        while not self._io.is_eof():
            self.members.append(Gzip.Member(self._io, self, self._root))
            i += 1


    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_bits_int_be(3)
            self.has_comment = self._io.read_bits_int_be(1) != 0
            self.has_name = self._io.read_bits_int_be(1) != 0
            self.has_extra = self._io.read_bits_int_be(1) != 0
            self.has_header_crc = self._io.read_bits_int_be(1) != 0
            self.is_text = self._io.read_bits_int_be(1) != 0


    class Strz(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.str = (self._io.read_bytes_term(0, False, True, False)).decode(u"UTF-8")


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_extra = self._io.read_u2le()
            self.extra = self._io.read_bytes(self.len_extra)


    class Member(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Gzip.Header(self._io, self, self._root)
            if self.header.flags.has_extra:
                self.extra = Gzip.ExtraField(self._io, self, self._root)

            if self.header.flags.has_name:
                self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.header.flags.has_comment:
                self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

            if self.header.flags.has_header_crc:
                self.header_crc16 = self._io.read_bytes(2)

            self.body = self._io.read_bytes_full()
            self.footer = Gzip.Footer(self._io, self, self._root)


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
            self.compression_method = self._io.read_bytes(1)
            if not self.compression_method == b"\x08":
                raise kaitaistruct.ValidationNotEqualError(b"\x08", self.compression_method, self._io, u"/types/header/seq/1")
            self.flags = Gzip.Flags(self._io, self, self._root)
            self.mod_time = self._io.read_u4le()
            self.extra_flags = self._io.read_u1()
            self.os = self._io.read_u1()


    class Footer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.input_size = self._io.read_u4le()



