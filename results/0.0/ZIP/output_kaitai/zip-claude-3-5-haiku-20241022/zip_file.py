# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ZipFile(KaitaiStruct):

    class SectionType(Enum):
        central_directory_file_header = 33639248
        local_file_header = 67324752
        end_of_central_directory = 101010256
        zip64_end_of_central_directory = 101075792

    class CompressionMethod(Enum):
        no_compression = 0
        shrink = 1
        reduce_factor1 = 2
        reduce_factor2 = 3
        reduce_factor3 = 4
        reduce_factor4 = 5
        implode = 6
        deflate = 8
        deflate64 = 9
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        lz77 = 19
        ppmd = 98
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.sections = []
        i = 0
        while not self._io.is_eof():
            self.sections.append(ZipFile.Section(self._io, self, self._root))
            i += 1


    class EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number_of_this_disk = self._io.read_u2le()
            self.number_of_disk_with_start_of_central_directory = self._io.read_u2le()
            self.total_entries_on_this_disk = self._io.read_u2le()
            self.total_entries_in_central_directory = self._io.read_u2le()
            self.size_of_central_directory = self._io.read_u4le()
            self.offset_of_start_of_central_directory = self._io.read_u4le()
            self.zip_file_comment_length = self._io.read_u2le()
            self.zip_file_comment = (self._io.read_bytes(self.zip_file_comment_length)).decode(u"UTF-8")


    class Zip64EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size_of_record = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.number_of_this_disk = self._io.read_u4le()
            self.number_of_disk_with_start_of_central_directory = self._io.read_u4le()
            self.total_entries_on_this_disk = self._io.read_u8le()
            self.total_entries_in_central_directory = self._io.read_u8le()
            self.size_of_central_directory = self._io.read_u8le()
            self.offset_of_start_of_central_directory = self._io.read_u8le()


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = KaitaiStream.resolve_enum(ZipFile.SectionType, self._io.read_u4le())
            _on = self.magic
            if _on == ZipFile.SectionType.local_file_header:
                self.body = ZipFile.LocalFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionType.central_directory_file_header:
                self.body = ZipFile.CentralDirectoryFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionType.end_of_central_directory:
                self.body = ZipFile.EndOfCentralDirectory(self._io, self, self._root)
            elif _on == ZipFile.SectionType.zip64_end_of_central_directory:
                self.body = ZipFile.Zip64EndOfCentralDirectory(self._io, self, self._root)


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(ZipFile.CompressionMethod, self._io.read_u2le())
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.compressed_data = self._io.read_bytes(self.compressed_size)


    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(ZipFile.CompressionMethod, self._io.read_u2le())
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_comment_length = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attributes = self._io.read_u2le()
            self.external_file_attributes = self._io.read_u4le()
            self.relative_offset_of_local_header = self._io.read_u4le()
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")



