# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):

    class LocalFileHeader(Enum):
        local_file_header = 67324752

    class CompressionMethods(Enum):
        stored = 0
        shrunk = 1
        reduced_1 = 2
        reduced_2 = 3
        reduced_3 = 4
        reduced_4 = 5
        imploded = 6
        tokenized = 7
        deflated = 8
        enhanced_deflated = 9
        pkware_dcl_imploded = 10
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        ibm_lz77_z = 19
        zstd = 97
        mp3 = 98
        xz = 99
        jpeg = 100
        wavpack = 101
        ppmd = 102
        aex_encryption_marker = 103
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.signature = KaitaiStream.resolve_enum(Zip.LocalFileHeader, self._io.read_u4le())
        self.version_needed = self._io.read_u2le()
        self.general_purpose_bit_flag = self._io.read_u2le()
        self.compression_method = KaitaiStream.resolve_enum(Zip.CompressionMethods, self._io.read_u2le())
        self.last_mod_time = self._io.read_u2le()
        self.last_mod_date = self._io.read_u2le()
        self.crc32 = self._io.read_u4le()
        self.compressed_size = self._io.read_u4le()
        self.uncompressed_size = self._io.read_u4le()
        self.file_name_length = self._io.read_u2le()
        self.extra_field_length = self._io.read_u2le()
        self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
        self.extra_field = []
        for i in range(self.extra_field_length):
            self.extra_field.append(self._io.read_u1())

        self.file_data = []
        for i in range(self.compressed_size):
            self.file_data.append(self._io.read_u1())



