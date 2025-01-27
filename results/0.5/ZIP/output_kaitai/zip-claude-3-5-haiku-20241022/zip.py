# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.signature = self._io.read_bytes(4)
        if not self.signature == b"\x50\x4B\x03\x04":
            raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x03\x04", self.signature, self._io, u"/seq/0")
        self.version_extract = self._io.read_u2le()
        self.flags = self._io.read_u2le()
        self.compression_method = self._io.read_u2le()
        self.last_mod_time = self._io.read_u2le()
        self.last_mod_date = self._io.read_u2le()
        self.crc32 = self._io.read_u4le()
        self.compressed_size = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()
        self.filename_length = self._io.read_u2le()
        self.extra_field_length = self._io.read_u2le()
        self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
        self.extra_field = self._io.read_bytes(self.extra_field_length)
        self.compressed_data = self._io.read_bytes(self.compressed_size)


