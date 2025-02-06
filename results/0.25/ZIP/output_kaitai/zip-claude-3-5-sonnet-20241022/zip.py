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
        jpeg_variant = 96
        wavpack = 97
        ppmd = 98
        aes_encrypted = 99
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

    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_u4le()
            if self.signature == 67324752:
                self.local_file = Zip.LocalFileHeader(self._io, self, self._root)

            if self.signature == 33639248:
                self.central_dir = Zip.CentralDirHeader(self._io, self, self._root)

            if self.signature == 101010256:
                self.end_of_central_dir = Zip.EndOfCentralDir(self._io, self, self._root)



    class LocalFileHeader(KaitaiStruct):
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

        @property
        def version_major(self):
            if hasattr(self, '_m_version_major'):
                return self._m_version_major

            self._m_version_major = (self.version_needed >> 8)
            return getattr(self, '_m_version_major', None)

        @property
        def is_strong_encrypted(self):
            if hasattr(self, '_m_is_strong_encrypted'):
                return self._m_is_strong_encrypted

            self._m_is_strong_encrypted = (self.flags & 64) != 0
            return getattr(self, '_m_is_strong_encrypted', None)

        @property
        def version_minor(self):
            if hasattr(self, '_m_version_minor'):
                return self._m_version_minor

            self._m_version_minor = (self.version_needed & 255)
            return getattr(self, '_m_version_minor', None)

        @property
        def has_data_descriptor(self):
            if hasattr(self, '_m_has_data_descriptor'):
                return self._m_has_data_descriptor

            self._m_has_data_descriptor = (self.flags & 8) != 0
            return getattr(self, '_m_has_data_descriptor', None)

        @property
        def has_utf8(self):
            if hasattr(self, '_m_has_utf8'):
                return self._m_has_utf8

            self._m_has_utf8 = (self.flags & 2048) != 0
            return getattr(self, '_m_has_utf8', None)

        @property
        def is_compressed_patched(self):
            if hasattr(self, '_m_is_compressed_patched'):
                return self._m_is_compressed_patched

            self._m_is_compressed_patched = (self.flags & 32) != 0
            return getattr(self, '_m_is_compressed_patched', None)

        @property
        def is_encrypted(self):
            if hasattr(self, '_m_is_encrypted'):
                return self._m_is_encrypted

            self._m_is_encrypted = (self.flags & 1) != 0
            return getattr(self, '_m_is_encrypted', None)


    class CentralDirHeader(KaitaiStruct):
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
            self.comment_len = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_attrs = self._io.read_u2le()
            self.external_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self.extra_field = self._io.read_bytes(self.extra_field_len)
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")

        @property
        def version_major(self):
            if hasattr(self, '_m_version_major'):
                return self._m_version_major

            self._m_version_major = (self.version_needed >> 8)
            return getattr(self, '_m_version_major', None)

        @property
        def is_strong_encrypted(self):
            if hasattr(self, '_m_is_strong_encrypted'):
                return self._m_is_strong_encrypted

            self._m_is_strong_encrypted = (self.flags & 64) != 0
            return getattr(self, '_m_is_strong_encrypted', None)

        @property
        def version_minor(self):
            if hasattr(self, '_m_version_minor'):
                return self._m_version_minor

            self._m_version_minor = (self.version_needed & 255)
            return getattr(self, '_m_version_minor', None)

        @property
        def has_data_descriptor(self):
            if hasattr(self, '_m_has_data_descriptor'):
                return self._m_has_data_descriptor

            self._m_has_data_descriptor = (self.flags & 8) != 0
            return getattr(self, '_m_has_data_descriptor', None)

        @property
        def has_utf8(self):
            if hasattr(self, '_m_has_utf8'):
                return self._m_has_utf8

            self._m_has_utf8 = (self.flags & 2048) != 0
            return getattr(self, '_m_has_utf8', None)

        @property
        def is_compressed_patched(self):
            if hasattr(self, '_m_is_compressed_patched'):
                return self._m_is_compressed_patched

            self._m_is_compressed_patched = (self.flags & 32) != 0
            return getattr(self, '_m_is_compressed_patched', None)

        @property
        def is_encrypted(self):
            if hasattr(self, '_m_is_encrypted'):
                return self._m_is_encrypted

            self._m_is_encrypted = (self.flags & 1) != 0
            return getattr(self, '_m_is_encrypted', None)


    class EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number = self._io.read_u2le()
            self.disk_start = self._io.read_u2le()
            self.num_entries_this_disk = self._io.read_u2le()
            self.num_entries_total = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.comment_len = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")



