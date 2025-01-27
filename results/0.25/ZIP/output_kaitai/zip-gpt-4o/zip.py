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


    class Zip64EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size_of_zip64_end_of_central_dir = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.disk_number = self._io.read_u4le()
            self.central_dir_disk = self._io.read_u4le()
            self.num_central_dir_records_on_disk = self._io.read_u8le()
            self.total_central_dir_records = self._io.read_u8le()
            self.central_dir_size = self._io.read_u8le()
            self.central_dir_offset = self._io.read_u8le()
            self.extensible_data_sector = self._io.read_bytes_full()


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
                self.body = Zip.CentralDirEntry(_io__raw_body, self, self._root)
            elif _on == 134695760:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.DataDescriptor(_io__raw_body, self, self._root)
            elif _on == 117853008:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.Zip64EndOfCentralDirLocator(_io__raw_body, self, self._root)
            elif _on == 101075792:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.Zip64EndOfCentralDir(_io__raw_body, self, self._root)
            elif _on == 101010256:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.EndOfCentralDir(_io__raw_body, self, self._root)
            elif _on == 67324752:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.LocalFileHeader(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes_full()


    class CentralDirEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.general_purpose_flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.file_mod_time = self._io.read_u2le()
            self.file_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_field_len = self._io.read_u2le()
            self.file_comment_len = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attrs = self._io.read_u2le()
            self.external_file_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_len)
            self.file_comment = (self._io.read_bytes(self.file_comment_len)).decode(u"UTF-8")


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = self._io.read_u2le()
            self.general_purpose_flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.file_mod_time = self._io.read_u2le()
            self.file_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_field_len = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_len)


    class Zip64EndOfCentralDirLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.central_dir_disk = self._io.read_u4le()
            self.zip64_end_of_central_dir_offset = self._io.read_u8le()
            self.num_disks = self._io.read_u4le()


    class EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number = self._io.read_u2le()
            self.central_dir_disk = self._io.read_u2le()
            self.num_central_dir_records_on_disk = self._io.read_u2le()
            self.total_central_dir_records = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.comment_len = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")



