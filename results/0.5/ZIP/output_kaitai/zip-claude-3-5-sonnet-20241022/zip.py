# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):

    class CompressionMethod(Enum):
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
        lz77 = 19
        xz = 95
        jpeg = 96
        wavpack = 97
        ppmd = 98
        aes = 99
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
            if self._io.is_eof():
                break
            i += 1

    class NtfsExtra(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_u4le()
            self.attributes = []
            i = 0
            while not self._io.is_eof():
                self.attributes.append(Zip.NtfsAttribute(self._io, self, self._root))
                i += 1



    class Zip64ExtendedInfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.original_size = self._io.read_u8le()
            self.compressed_size = self._io.read_u8le()
            self.local_header_offset = self._io.read_u8le()
            self.disk_start = self._io.read_u4le()


    class LocalFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = self._io.read_u2le()
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.len_body = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.len_file_name = self._io.read_u2le()
            self.len_extra_field = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.len_file_name)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.len_extra_field)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)
            self.body = self._io.read_bytes(self.len_body)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while not self._io.is_eof():
                self.entries.append(Zip.ExtraFieldEntry(self._io, self, self._root))
                i += 1



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
            self.compression_method = self._io.read_u2le()
            self.last_mod_time = self._io.read_u2le()
            self.last_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.len_file_name = self._io.read_u2le()
            self.len_extra_field = self._io.read_u2le()
            self.len_file_comment = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attr = self._io.read_u2le()
            self.external_file_attr = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.len_file_name)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.len_extra_field)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)
            self.file_comment = (self._io.read_bytes(self.len_file_comment)).decode(u"UTF-8")


    class ExtraFieldEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_id = self._io.read_u2le()
            self.len_data = self._io.read_u2le()
            _on = self.header_id
            if _on == 10:
                self._raw_data = self._io.read_bytes(self.len_data)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Zip.NtfsExtra(_io__raw_data, self, self._root)
            elif _on == 30837:
                self._raw_data = self._io.read_bytes(self.len_data)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Zip.UnixExtra(_io__raw_data, self, self._root)
            elif _on == 1:
                self._raw_data = self._io.read_bytes(self.len_data)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Zip.Zip64ExtendedInfo(_io__raw_data, self, self._root)
            elif _on == 21589:
                self._raw_data = self._io.read_bytes(self.len_data)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Zip.ExtendedTimestamp(_io__raw_data, self, self._root)
            else:
                self._raw_data = self._io.read_bytes(self.len_data)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Zip.RawExtraField(_io__raw_data, self, self._root)


    class ExtendedTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flags = self._io.read_u1()
            if (self.flags & 1) != 0:
                self.mod_time = self._io.read_u4le()

            if (self.flags & 2) != 0:
                self.access_time = self._io.read_u4le()

            if (self.flags & 4) != 0:
                self.create_time = self._io.read_u4le()



    class UnixExtra(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = self._io.read_u1()
            self.len_uid = self._io.read_u1()
            self.uid = self._io.read_bytes(self.len_uid)
            self.len_gid = self._io.read_u1()
            self.gid = self._io.read_bytes(self.len_gid)


    class RawExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class NtfsAttribute(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u2le()
            self.len_data = self._io.read_u2le()
            self.data = self._io.read_bytes(self.len_data)


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
            self.len_comment = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.len_comment)).decode(u"UTF-8")



