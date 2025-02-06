# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ZipFile(KaitaiStruct):

    class SectionSignatures(Enum):
        central_directory_file_header = 33639248
        local_file_header = 67324752
        end_of_central_directory = 101010256
        zip64_end_of_central_directory = 101075792

    class GeneralPurposeFlags(Enum):
        encrypted = 1
        compression_option_1 = 2
        compression_option_2 = 4
        data_descriptor_present = 8
        enhanced_deflation = 16
        compressed_patched_data = 32

    class CompressionMethods(Enum):
        no_compression = 0
        shrunk = 1
        reduced_factor1 = 2
        reduced_factor2 = 3
        reduced_factor3 = 4
        reduced_factor4 = 5
        imploded = 6
        deflated = 8
        enhanced_deflated = 9
        pkware_dcl_imploded = 10
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        ibm_lz77_z = 19
        blowfish = 98
        twofish = 99
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
            self.disk_where_central_directory_starts = self._io.read_u2le()
            self.number_of_central_directory_records_on_this_disk = self._io.read_u2le()
            self.total_number_of_central_directory_records = self._io.read_u2le()
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
            self.size_of_zip64_end_of_central_directory_record = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.number_of_this_disk = self._io.read_u4le()
            self.disk_where_central_directory_starts = self._io.read_u4le()
            self.number_of_central_directory_records_on_this_disk = self._io.read_u8le()
            self.total_number_of_central_directory_records = self._io.read_u8le()
            self.size_of_central_directory = self._io.read_u8le()
            self.offset_of_start_of_central_directory = self._io.read_u8le()


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extra_field_data = []
            i = 0
            while not self._io.is_eof():
                self.extra_field_data.append(self._io.read_u1())
                i += 1



    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.section_type = KaitaiStream.resolve_enum(ZipFile.SectionSignatures, self._io.read_u4le())
            _on = self.section_type
            if _on == ZipFile.SectionSignatures.local_file_header:
                self.content = ZipFile.LocalFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.central_directory_file_header:
                self.content = ZipFile.CentralDirectoryFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.end_of_central_directory:
                self.content = ZipFile.EndOfCentralDirectory(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.zip64_end_of_central_directory:
                self.content = ZipFile.Zip64EndOfCentralDirectory(self._io, self, self._root)


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = KaitaiStream.resolve_enum(ZipFile.GeneralPurposeFlags, self._io.read_u2le())
            self.compression_method = KaitaiStream.resolve_enum(ZipFile.CompressionMethods, self._io.read_u2le())
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = ZipFile.ExtraField(_io__raw_extra_field, self, self._root)
            self._raw_compressed_data = self._io.read_bytes(self.compressed_size)
            _io__raw_compressed_data = KaitaiStream(BytesIO(self._raw_compressed_data))
            self.compressed_data = ZipFile.CompressedData(_io__raw_compressed_data, self, self._root)


    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = KaitaiStream.resolve_enum(ZipFile.GeneralPurposeFlags, self._io.read_u2le())
            self.compression_method = KaitaiStream.resolve_enum(ZipFile.CompressionMethods, self._io.read_u2le())
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
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = ZipFile.ExtraField(_io__raw_extra_field, self, self._root)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")


    class CompressedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = []
            i = 0
            while not self._io.is_eof():
                self.data.append(self._io.read_u1())
                i += 1




