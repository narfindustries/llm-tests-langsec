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
        self.local_file_header = Zip.LocalFileHeader(self._io, self, self._root)
        self.file_data = self._io.read_bytes(self.local_file_header.compressed_size)
        if (self.local_file_header.general_purpose_bit_flag & 8) != 0:
            self.data_descriptor = Zip.DataDescriptor(self._io, self, self._root)

        self.central_directory = []
        i = 0
        while not self._io.is_eof():
            self.central_directory.append(Zip.CentralDirectory(self._io, self, self._root))
            i += 1

        self.end_of_central_directory = Zip.EndOfCentralDirectory(self._io, self, self._root)

    class Zip64EndOfCentralDirectoryLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x06\x07":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x06\x07", self.signature, self._io, u"/types/zip64_end_of_central_directory_locator/seq/0")
            self.disk_where_zip64_end_of_central_directory_record_starts = self._io.read_u4le()
            self.relative_offset_of_zip64_end_of_central_directory_record = self._io.read_u8le()
            self.total_number_of_disks = self._io.read_u4le()


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
            self.number_of_this_disk = self._io.read_u2le()
            self.disk_where_central_directory_starts = self._io.read_u2le()
            self.number_of_central_directory_records_on_this_disk = self._io.read_u2le()
            self.total_number_of_central_directory_records = self._io.read_u2le()
            self.size_of_central_directory = self._io.read_u4le()
            self.offset_of_start_of_central_directory = self._io.read_u4le()
            self.zip_file_comment_length = self._io.read_u2le()
            self.zip_file_comment = (self._io.read_bytes(self.zip_file_comment_length)).decode(u"ASCII")


    class DataDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x07\x08":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x07\x08", self.signature, self._io, u"/types/data_descriptor/seq/0")
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()


    class CentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x01\x02":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x01\x02", self.signature, self._io, u"/types/central_directory/seq/0")
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_comment_length = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attributes = self._io.read_u2le()
            self.external_file_attributes = self._io.read_u4le()
            self.relative_offset_of_local_header = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"ASCII")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"ASCII")


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
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"ASCII")
            self.extra_field = self._io.read_bytes(self.extra_field_length)


    class Zip64EndOfCentralDirectoryRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x06\x06":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x06\x06", self.signature, self._io, u"/types/zip64_end_of_central_directory_record/seq/0")
            self.size_of_zip64_end_of_central_directory_record = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.number_of_this_disk = self._io.read_u4le()
            self.disk_where_central_directory_starts = self._io.read_u4le()
            self.number_of_central_directory_records_on_this_disk = self._io.read_u8le()
            self.total_number_of_central_directory_records = self._io.read_u8le()
            self.size_of_central_directory = self._io.read_u8le()
            self.offset_of_start_of_central_directory = self._io.read_u8le()



