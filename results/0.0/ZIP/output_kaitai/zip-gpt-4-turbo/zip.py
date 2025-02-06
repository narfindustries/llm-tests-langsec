# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):
    """ZIP is a popular archive file format that supports lossless data compression.
    A ZIP file may contain one or more files or directories that may have been compressed.
    The ZIP file format permits a number of compression algorithms.
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


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = self._io.read_u4le()
            _on = self.header
            if _on == 67324752:
                self.body = Zip.LocalFileHeader(self._io, self, self._root)
            elif _on == 33639248:
                self.body = Zip.CentralDirectoryFileHeader(self._io, self, self._root)
            elif _on == 101010256:
                self.body = Zip.EndOfCentralDirectoryRecord(self._io, self, self._root)


    class LocalFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed = self._io.read_u2le()
            self.gp_bit_flag = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)


    class CentralDirectoryFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.gp_bit_flag = self._io.read_u2le()
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
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_length)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")


    class EndOfCentralDirectoryRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_num = self._io.read_u2le()
            self.cd_start_disk = self._io.read_u2le()
            self.num_cd_records_on_disk = self._io.read_u2le()
            self.total_cd_records = self._io.read_u2le()
            self.cd_size = self._io.read_u4le()
            self.cd_offset = self._io.read_u4le()
            self.zip_comment_len = self._io.read_u2le()
            self.zip_comment = (self._io.read_bytes(self.zip_comment_len)).decode(u"UTF-8")



