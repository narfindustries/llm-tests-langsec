# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):

    class Compression(Enum):
        none = 0
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
        ibm_lz77 = 19

    class Flags(Enum):
        encrypted = 1
        compression_option1 = 2
        compression_option2 = 4
        data_descriptor = 8
        enhanced_deflation = 16
        compressed_patched = 32
        strong_encryption = 64
        utf8 = 2048
        mask_header_values = 8192
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

    class LocalFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.Compression, self._io.read_u2le())
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_field_len = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_len)
            self.body = self._io.read_bytes(self.compressed_size)


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


    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            _on = self.signature
            if _on == 67324752:
                self.body = Zip.LocalFile(self._io, self, self._root)
            elif _on == 33639248:
                self.body = Zip.CentralDirEntry(self._io, self, self._root)
            elif _on == 101010256:
                self.body = Zip.EndOfCentralDir(self._io, self, self._root)


    class CentralDirEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.Compression, self._io.read_u2le())
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_field_len = self._io.read_u2le()
            self.file_comment_len = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_attrs = self._io.read_u2le()
            self.external_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_len)
            self.file_comment = (self._io.read_bytes(self.file_comment_len)).decode(u"UTF-8")


    class EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number = self._io.read_u2le()
            self.disk_cd_start = self._io.read_u2le()
            self.num_entries_disk = self._io.read_u2le()
            self.num_entries_total = self._io.read_u2le()
            self.cd_size = self._io.read_u4le()
            self.cd_offset = self._io.read_u4le()
            self.comment_len = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")



