# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):
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
        self.compression_method = self._io.read_u1()
        if not self.compression_method == 8:
            raise kaitaistruct.ValidationNotEqualError(8, self.compression_method, self._io, u"/seq/2")
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.os = self._io.read_u1()
        if self.flags.fextra:
            self.extra = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.fname:
            self.original_file_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.fcomment:
            self.file_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if self.flags.fhcrc:
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
            self.ftext = self._io.read_bits_int_be(1) != 0
            self.fhcrc = self._io.read_bits_int_be(1) != 0
            self.fextra = self._io.read_bits_int_be(1) != 0
            self.fname = self._io.read_bits_int_be(1) != 0
            self.fcomment = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
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



