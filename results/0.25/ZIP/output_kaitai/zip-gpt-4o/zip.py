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
        self.sections = []
        i = 0
        while True:
            _ = Zip.Section(self._io, self, self._root)
            self.sections.append(_)
            if _.signature == 101010256:
                break
            i += 1

    class DigitalSignature(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size_of_data = self._io.read_u2le()
            self.signature_data = self._io.read_bytes(self.size_of_data)


    class Zip64EndOfCentralDirectoryLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number_of_the_disk_with_start_of_zip64_end_of_central_directory = self._io.read_u4le()
            self.relative_offset_of_zip64_end_of_central_directory_record = self._io.read_u8le()
            self.total_number_of_disks = self._io.read_u4le()


    class ArchiveExtraDataRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length_of_data = self._io.read_u4le()
            self.extra_field_data = self._io.read_bytes(self.length_of_data)


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            _on = self.signature
            if _on == 33639248:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.CentralDirectoryFileHeader(_io__raw_body, self, self._root)
            elif _on == 134630224:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.ArchiveExtraDataRecord(_io__raw_body, self, self._root)
            elif _on == 117853008:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.Zip64EndOfCentralDirectoryLocator(_io__raw_body, self, self._root)
            elif _on == 84233040:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.DigitalSignature(_io__raw_body, self, self._root)
            elif _on == 101075792:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.Zip64EndOfCentralDirectoryRecord(_io__raw_body, self, self._root)
            elif _on == 101010256:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.EndOfCentralDirectoryRecord(_io__raw_body, self, self._root)
            elif _on == 67324752:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.LocalFileHeader(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes_full()


    class EndOfCentralDirectoryRecord(KaitaiStruct):
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
            self.zip_file_comment = self._io.read_bytes(self.zip_file_comment_length)


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
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
            self.file_name = self._io.read_bytes(self.file_name_length)
            self.extra_field = self._io.read_bytes(self.extra_field_length)


    class Zip64EndOfCentralDirectoryRecord(KaitaiStruct):
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
            self.file_name = self._io.read_bytes(self.file_name_length)
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = self._io.read_bytes(self.file_comment_length)



