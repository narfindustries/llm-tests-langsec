# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ZipFile(KaitaiStruct):

    class SectionSignatures(Enum):
        central_file_header = 33639248
        local_file_header = 67324752
        end_of_central_directory = 101010256
        zip64_end_of_central_directory_locator = 101010272
        zip64_end_of_central_directory = 101075792

    class CompressionMethods(Enum):
        no_compression = 0
        shrunk = 1
        reduced_factor1 = 2
        reduced_factor2 = 3
        reduced_factor3 = 4
        reduced_factor4 = 5
        imploded = 6
        deflated = 8
        enhanced_deflate = 9
        pkware_dcl_imploded = 10
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        ibm_lz77_z = 19
        blech = 98
        winzip_aes = 99
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
            self.number_of_disk_with_start_of_zip64_eocd = self._io.read_u4le()
            self.relative_offset_of_zip64_eocd = self._io.read_u8le()
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
            self.disk_with_central_directory = self._io.read_u2le()
            self.total_entries_central_directory_this_disk = self._io.read_u2le()
            self.total_entries_central_directory = self._io.read_u2le()
            self.size_of_central_directory = self._io.read_u4le()
            self.offset_to_central_directory = self._io.read_u4le()
            self.zip_file_comment_length = self._io.read_u2le()
            self.zip_file_comment = (self._io.read_bytes(self.zip_file_comment_length)).decode(u"ascii")


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
            self.size_of_record = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.number_of_this_disk = self._io.read_u4le()
            self.disk_with_central_directory = self._io.read_u4le()
            self.total_entries_central_directory_this_disk = self._io.read_u8le()
            self.total_entries_central_directory = self._io.read_u8le()
            self.size_of_central_directory = self._io.read_u8le()
            self.offset_to_central_directory = self._io.read_u8le()


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.section_type = KaitaiStream.resolve_enum(ZipFile.SectionSignatures, self._io.read_u4le())

        @property
        def section_data(self):
            if hasattr(self, '_m_section_data'):
                return self._m_section_data

            io = self._root._io
            _pos = io.pos()
            io.seek(self._io.pos())
            _on = self.section_type
            if _on == ZipFile.SectionSignatures.central_file_header:
                self._m_section_data = ZipFile.CentralFileHeader(io, self, self._root)
            elif _on == ZipFile.SectionSignatures.local_file_header:
                self._m_section_data = ZipFile.LocalFileHeader(io, self, self._root)
            elif _on == ZipFile.SectionSignatures.zip64_end_of_central_directory:
                self._m_section_data = ZipFile.Zip64EndOfCentralDirectory(io, self, self._root)
            elif _on == ZipFile.SectionSignatures.zip64_end_of_central_directory_locator:
                self._m_section_data = ZipFile.Zip64EndOfCentralDirectoryLocator(io, self, self._root)
            elif _on == ZipFile.SectionSignatures.end_of_central_directory:
                self._m_section_data = ZipFile.EndOfCentralDirectory(io, self, self._root)
            io.seek(_pos)
            return getattr(self, '_m_section_data', None)


    class CentralFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x01\x02":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x01\x02", self.signature, self._io, u"/types/central_file_header/seq/0")
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.general_purpose_bit_flag = self._io.read_u2le()
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
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"ascii")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"ascii")


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
            self.compression_method = KaitaiStream.resolve_enum(ZipFile.CompressionMethods, self._io.read_u2le())
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"ascii")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.body = self._io.read_bytes(self.compressed_size)



