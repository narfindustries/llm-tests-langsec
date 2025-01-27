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
        self.signature = self._io.read_u4le()
        if not self.signature == 67324752:
            raise kaitaistruct.ValidationNotEqualError(67324752, self.signature, self._io, u"/seq/0")
        self.version_needed = self._io.read_u2le()
        self.flags = self._io.read_u2le()
        self.compression_method = self._io.read_u2le()
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

        self.data = []
        for i in range(self.compressed_size):
            self.data.append(self._io.read_u1())


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            if not self.signature == 67324752:
                raise kaitaistruct.ValidationNotEqualError(67324752, self.signature, self._io, u"/types/local_file_header/seq/0")
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
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

            self.data = []
            for i in range(self.compressed_size):
                self.data.append(self._io.read_u1())



    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            if not self.signature == 33639248:
                raise kaitaistruct.ValidationNotEqualError(33639248, self.signature, self._io, u"/types/central_directory_file_header/seq/0")
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
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
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self.extra_field = []
            for i in range(self.extra_field_length):
                self.extra_field.append(self._io.read_u1())

            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")


    class EndOfCentralDirectoryRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            if not self.signature == 101010256:
                raise kaitaistruct.ValidationNotEqualError(101010256, self.signature, self._io, u"/types/end_of_central_directory_record/seq/0")
            self.disk_number = self._io.read_u2le()
            self.disk_number_with_cd = self._io.read_u2le()
            self.num_entries_on_disk = self._io.read_u2le()
            self.num_entries_total = self._io.read_u2le()
            self.size_of_cd = self._io.read_u4le()
            self.offset_of_cd = self._io.read_u4le()
            self.comment_length = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_length)).decode(u"UTF-8")


    @property
    def local_file_headers(self):
        if hasattr(self, '_m_local_file_headers'):
            return self._m_local_file_headers

        self._m_local_file_headers = []
        i = 0
        while not self._io.is_eof():
            self._m_local_file_headers.append(Zip.LocalFileHeader(self._io, self, self._root))
            i += 1

        return getattr(self, '_m_local_file_headers', None)

    @property
    def central_directory_file_headers(self):
        if hasattr(self, '_m_central_directory_file_headers'):
            return self._m_central_directory_file_headers

        self._m_central_directory_file_headers = []
        i = 0
        while not self._io.is_eof():
            self._m_central_directory_file_headers.append(Zip.CentralDirectoryFileHeader(self._io, self, self._root))
            i += 1

        return getattr(self, '_m_central_directory_file_headers', None)

    @property
    def end_of_central_directory_record(self):
        if hasattr(self, '_m_end_of_central_directory_record'):
            return self._m_end_of_central_directory_record

        self._m_end_of_central_directory_record = []
        i = 0
        while not self._io.is_eof():
            self._m_end_of_central_directory_record.append(Zip.EndOfCentralDirectoryRecord(self._io, self, self._root))
            i += 1

        return getattr(self, '_m_end_of_central_directory_record', None)


