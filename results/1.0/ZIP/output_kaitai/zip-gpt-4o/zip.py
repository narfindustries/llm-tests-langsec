# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):

    class CompressionMethod(Enum):
        none = 0
        shrunk = 1
        deflated = 8
        enhanced_deflated = 9
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.files = []
        i = 0
        while True:
            _ = Zip.File(self._io, self, self._root)
            self.files.append(_)
            if _.signature != 67324752:
                break
            i += 1
        self.central_directory = []
        i = 0
        while not self._io.is_eof():
            self.central_directory.append(Zip.CentralDirectoryFileHeader(self._io, self, self._root))
            i += 1


    class EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            self.disk_num = self._io.read_u2le()
            self.central_dir_start_disk = self._io.read_u2le()
            self.num_records_on_disk = self._io.read_u2le()
            self.total_central_dir_records = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.len_comment = self._io.read_u2le()
            self.comment = self._io.read_bytes(self.len_comment)


    class DataDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()


    class Zip64EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            self.size_of_zip64_eocd = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.disk_num = self._io.read_u4le()
            self.central_dir_start_disk = self._io.read_u4le()
            self.num_records_on_disk = self._io.read_u8le()
            self.total_central_dir_records = self._io.read_u8le()
            self.central_dir_size = self._io.read_u8le()
            self.central_dir_offset = self._io.read_u8le()
            self.extensible_data_sector = self._io.read_bytes((self.size_of_zip64_eocd - 44))


    class File(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.len_file_name = self._io.read_u2le()
            self.len_extra_field = self._io.read_u2le()
            self.file_name = self._io.read_bytes(self.len_file_name)
            self.extra_field = self._io.read_bytes(self.len_extra_field)
            self.body = self._io.read_bytes(self.compressed_size)
            if (self.flags & 8) != 0:
                self.data_descriptor = Zip.DataDescriptor(self._io, self, self._root)



    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.len_file_name = self._io.read_u2le()
            self.len_extra_field = self._io.read_u2le()
            self.len_file_comment = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attr = self._io.read_u2le()
            self.external_file_attr = self._io.read_u4le()
            self.relative_offset = self._io.read_u4le()
            self.file_name = self._io.read_bytes(self.len_file_name)
            self.extra_field = self._io.read_bytes(self.len_extra_field)
            self.file_comment = self._io.read_bytes(self.len_file_comment)



