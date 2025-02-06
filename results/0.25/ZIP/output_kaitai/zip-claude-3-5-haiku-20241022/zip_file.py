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
        digital_signature = 84233040
        end_of_central_directory = 101010256
        zip64_end_of_central_directory_record = 101075792
        zip64_end_of_central_directory_locator = 117853008

    class CompressionMethods(Enum):
        no_compression = 0
        shrunk = 1
        reduced_1 = 2
        reduced_2 = 3
        reduced_3 = 4
        reduced_4 = 5
        imploded = 6
        deflated = 8
        enhanced_deflated = 9
        pkware_dcl_imploded = 10
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        lz77 = 19
        wavpack = 97
        ppmd = 98
        experimental = 99
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


    class DigitalSignature(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature_length = self._io.read_u2le()
            self.signature_data = self._io.read_bytes(self.signature_length)


    class Zip64EndOfCentralDirectoryLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number_of_disk_with_start_of_zip64_eocdr = self._io.read_u4le()
            self.relative_offset_of_zip64_eocdr = self._io.read_u8le()
            self.total_number_of_disks = self._io.read_u4le()


    class EndOfCentralDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number_of_this_disk = self._io.read_u2le()
            self.disk_where_central_directory_starts = self._io.read_u2le()
            self.total_central_directory_entries_on_this_disk = self._io.read_u2le()
            self.total_central_directory_entries = self._io.read_u2le()
            self.size_of_central_directory = self._io.read_u4le()
            self.offset_of_central_directory = self._io.read_u4le()
            self.zip_file_comment_length = self._io.read_u2le()
            self.zip_file_comment = (self._io.read_bytes(self.zip_file_comment_length)).decode(u"UTF-8")


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = KaitaiStream.resolve_enum(ZipFile.SectionSignatures, self._io.read_u4le())
            _on = self.signature
            if _on == ZipFile.SectionSignatures.central_directory_file_header:
                self.body = ZipFile.CentralDirectoryFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.digital_signature:
                self.body = ZipFile.DigitalSignature(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.zip64_end_of_central_directory_record:
                self.body = ZipFile.Zip64EndOfCentralDirectoryRecord(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.local_file_header:
                self.body = ZipFile.LocalFileHeader(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.zip64_end_of_central_directory_locator:
                self.body = ZipFile.Zip64EndOfCentralDirectoryLocator(self._io, self, self._root)
            elif _on == ZipFile.SectionSignatures.end_of_central_directory:
                self.body = ZipFile.EndOfCentralDirectory(self._io, self, self._root)


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
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
            self.filename = (self._io.read_bytes(self.filename_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)


    class Zip64EndOfCentralDirectoryRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size_of_zip64_eocdr = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.number_of_this_disk = self._io.read_u4le()
            self.number_of_disk_with_start_of_central_directory = self._io.read_u4le()
            self.total_entries_on_this_disk = self._io.read_u8le()
            self.total_entries = self._io.read_u8le()
            self.size_of_central_directory = self._io.read_u8le()
            self.offset_of_central_directory = self._io.read_u8le()
            self.extensible_data_sector = self._io.read_bytes_full()


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
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")



