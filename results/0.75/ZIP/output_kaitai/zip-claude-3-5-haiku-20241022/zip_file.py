# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ZipFile(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.sections = ZipFile.FileSections(self._io, self, self._root)

    class EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x05\x06":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x05\x06", self.signature, self._io, u"/types/end_of_central_directory/seq/0")
            self.disk_number = self._io.read_u2le()
            self.disk_central_dir = self._io.read_u2le()
            self.records_on_disk = self._io.read_u2le()
            self.total_records = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.comment_length = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_length)).decode(u"UTF-8")


    class Zip64EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x06\x06":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x06\x06", self.signature, self._io, u"/types/zip64_end_of_central_directory/seq/0")
            self.record_size = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_extract = self._io.read_u2le()
            self.disk_number = self._io.read_u4le()
            self.disk_central_dir = self._io.read_u4le()
            self.records_on_disk = self._io.read_u8le()
            self.total_records = self._io.read_u8le()
            self.central_dir_size = self._io.read_u8le()
            self.central_dir_offset = self._io.read_u8le()


    class CentralDirectoryHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x01\x02":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x01\x02", self.signature, self._io, u"/types/central_directory_header/seq/0")
            self.version_made_by = self._io.read_u2le()
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
            self.file_comment_length = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attrs = self._io.read_u2le()
            self.external_file_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")


    class FileSections(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while not self._io.is_eof():
                self.entries.append(ZipFile.LocalFileHeader(self._io, self, self._root))
                i += 1



    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x03\x04":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x03\x04", self.signature, self._io, u"/types/local_file_header/seq/0")
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



