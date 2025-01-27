# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class GzipDeepseekChat(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(2)
        if not self.magic == b"\x1F\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F\x8B", self.magic, self._io, u"/seq/0")
        self.compression_method = self._io.read_u1()
        self.flags = self._io.read_u1()
        self.modification_time = self._io.read_u4le()
        self.extra_flags = self._io.read_u1()
        self.os = self._io.read_u1()
        if (self.flags & 4) != 0:
            self.extra_field = GzipDeepseekChat.ExtraField(self._io, self, self._root)

        if (self.flags & 8) != 0:
            self.original_filename = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flags & 16) != 0:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")

        if (self.flags & 2) != 0:
            self.header_crc = self._io.read_u2le()

        self.compressed_data = GzipDeepseekChat.CompressedData(self._io, self, self._root)
        self.crc32 = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()

    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2le()
            self.data = self._io.read_bytes(self.length)


    class CompressedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()



