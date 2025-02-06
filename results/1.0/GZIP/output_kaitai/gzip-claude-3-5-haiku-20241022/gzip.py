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
        self.identification1 = self._io.read_u1()
        if not self.identification1 == 31:
            raise kaitaistruct.ValidationNotEqualError(31, self.identification1, self._io, u"/seq/0")
        self.identification2 = self._io.read_u1()
        if not self.identification2 == 139:
            raise kaitaistruct.ValidationNotEqualError(139, self.identification2, self._io, u"/seq/1")
        self.compression_method = self._io.read_u1()
        if not self.compression_method == 8:
            raise kaitaistruct.ValidationNotEqualError(8, self.compression_method, self._io, u"/seq/2")
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.modification_time = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.operating_system = self._io.read_u1()
        if self.flags.extra_field:
            self.extra_fields = Gzip.ExtraField(self._io, self, self._root)

        if self.flags.filename:
            self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.file_comment:
            self.file_comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        if self.flags.header_crc:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = Gzip.DeflateBlock(self._io, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text = self._io.read_bits_int_be(1) != 0
            self.header_crc = self._io.read_bits_int_be(1) != 0
            self.extra_field = self._io.read_bits_int_be(1) != 0
            self.filename = self._io.read_bits_int_be(1) != 0
            self.file_comment = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.total_length = self._io.read_u2le()
            self.subfields = []
            for i in range(self.total_length):
                self.subfields.append(Gzip.Subfield(self._io, self, self._root))



    class Subfield(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = (self._io.read_bytes(2)).decode(u"ASCII")
            self.length = self._io.read_u2le()
            self.data = self._io.read_bytes(self.length)


    class DeflateBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.raw_data = []
            i = 0
            while not self._io.is_eof():
                self.raw_data.append(self._io.read_u1())
                i += 1




