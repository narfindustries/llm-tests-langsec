# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):
    """ZIP is a popular archive file format, used to compress and bundle multiple files together.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.sections = []
        i = 0
        while not self._io.is_eof():
            self.sections.append(Zip.Section(self._io, self, self._root))
            i += 1


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


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Zip.LocalFileHeader(self._io, self, self._root)
            self.body = self._io.read_bytes(self.header.compressed_size)
            if (self.header.general_purpose_bit_flag & 8) != 0:
                self.descriptor = Zip.DataDescriptor(self._io, self, self._root)



    class DosDate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.year = self._io.read_bits_int_be(7)
            self.month = self._io.read_bits_int_be(4)
            self.day = self._io.read_bits_int_be(5)


    class DosTime(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.hour = self._io.read_bits_int_be(5)
            self.minute = self._io.read_bits_int_be(6)
            self.second = self._io.read_bits_int_be(5)


    class EndOfCentralDirectoryRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x05\x06":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x05\x06", self.signature, self._io, u"/types/end_of_central_directory_record/seq/0")
            self.number_of_this_disk = self._io.read_u2le()
            self.disk_where_central_directory_starts = self._io.read_u2le()
            self.number_of_central_directory_records_on_this_disk = self._io.read_u2le()
            self.total_number_of_central_directory_records = self._io.read_u2le()
            self.size_of_central_directory = self._io.read_u4le()
            self.offset_of_start_of_central_directory_relative_to_start_of_archive = self._io.read_u4le()
            self.zip_file_comment_length = self._io.read_u2le()
            self.zip_file_comment = (self._io.read_bytes(self.zip_file_comment_length)).decode(u"CP437")


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
            self.last_mod_file_time = Zip.DosTime(self._io, self, self._root)
            self.last_mod_file_date = Zip.DosDate(self._io, self, self._root)
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"CP437")
            self.extra_field = self._io.read_bytes(self.extra_field_length)


    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x01\x02":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x01\x02", self.signature, self._io, u"/types/central_directory_file_header/seq/0")
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = Zip.DosTime(self._io, self, self._root)
            self.last_mod_file_date = Zip.DosDate(self._io, self, self._root)
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
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"CP437")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"CP437")



