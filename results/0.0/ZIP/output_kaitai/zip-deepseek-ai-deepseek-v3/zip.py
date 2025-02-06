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
        self.local_files = []
        i = 0
        while not self._io.is_eof():
            self.local_files.append(Zip.LocalFile(self._io, self, self._root))
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
            self.disk_number = self._io.read_u4le()
            self.offset_of_record = self._io.read_u8le()
            self.total_disks = self._io.read_u4le()


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
            self.start_disk = self._io.read_u2le()
            self.entries_on_disk = self._io.read_u2le()
            self.total_entries = self._io.read_u2le()
            self.size_of_cd = self._io.read_u4le()
            self.offset_of_cd = self._io.read_u4le()
            self.comment_length = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_length)).decode(u"UTF-8")


    class LocalFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(4)
            if not self.signature == b"\x50\x4B\x03\x04":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x03\x04", self.signature, self._io, u"/types/local_file/seq/0")
            self.version_needed = self._io.read_u2le()
            self.general_purpose = self._io.read_u2le()
            self.compression = self._io.read_u2le()
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)


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
            self.version_needed = self._io.read_u2le()
            self.disk_number = self._io.read_u4le()
            self.start_disk = self._io.read_u4le()
            self.entries_on_disk = self._io.read_u8le()
            self.total_entries = self._io.read_u8le()
            self.size_of_cd = self._io.read_u8le()
            self.offset_of_cd = self._io.read_u8le()


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_id = self._io.read_u2le()
            self.data_size = self._io.read_u2le()
            self.data = self._io.read_bytes(self.data_size)


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
            self.version_needed = self._io.read_u2le()
            self.general_purpose = self._io.read_u2le()
            self.compression = self._io.read_u2le()
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_comment_length = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_attributes = self._io.read_u2le()
            self.external_attributes = self._io.read_u4le()
            self.relative_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")



