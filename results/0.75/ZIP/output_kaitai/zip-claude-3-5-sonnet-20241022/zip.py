# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Zip(KaitaiStruct):

    class MagicType(Enum):
        central_dir = 33639248
        local_file = 67324752
        end_of_central_dir = 101010256
        zip64_end_of_central_dir = 101075792
        zip64_end_of_central_dir_locator = 117853008

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
        ibm_terse = 18
        ibm_lz77 = 19
        wavpack = 97
        ppmd = 98
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.sections = []
        i = 0
        while True:
            _ = Zip.PkSection(self._io, self, self._root)
            self.sections.append(_)
            if self._io.is_eof():
                break
            i += 1

    class CentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.CompressionMethod, self._io.read_u2le())
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
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)
            self.file_comment = (self._io.read_bytes(self.file_comment_length)).decode(u"UTF-8")


    class LocalFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed_to_extract = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.CompressionMethod, self._io.read_u2le())
            self.last_mod_file_time = self._io.read_u2le()
            self.last_mod_file_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_length = self._io.read_u2le()
            self.extra_field_length = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_length)).decode(u"UTF-8")
            self._raw_extra_field = self._io.read_bytes(self.extra_field_length)
            _io__raw_extra_field = KaitaiStream(BytesIO(self._raw_extra_field))
            self.extra_field = Zip.ExtraField(_io__raw_extra_field, self, self._root)
            self.body = self._io.read_bytes(self.compressed_size)
            if (self.flags & 8) != 0:
                self.data_descriptor = Zip.DataDescriptor(self._io, self, self._root)



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
            self.record_size = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.disk_number = self._io.read_u4le()
            self.disk_start_central_dir = self._io.read_u4le()
            self.num_entries_this_disk = self._io.read_u8le()
            self.num_entries = self._io.read_u8le()
            self.central_dir_size = self._io.read_u8le()
            self.central_dir_offset = self._io.read_u8le()
            self.zip64_extensible_data = self._io.read_bytes((self.record_size - 44))


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



    class PkSection(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = KaitaiStream.resolve_enum(Zip.MagicType, self._io.read_u4le())
            _on = self.magic
            if _on == Zip.MagicType.zip64_end_of_central_dir_locator:
                self.body = Zip.Zip64EndOfCentralDirLocator(self._io, self, self._root)
            elif _on == Zip.MagicType.central_dir:
                self.body = Zip.CentralDir(self._io, self, self._root)
            elif _on == Zip.MagicType.zip64_end_of_central_dir:
                self.body = Zip.Zip64EndOfCentralDir(self._io, self, self._root)
            elif _on == Zip.MagicType.local_file:
                self.body = Zip.LocalFile(self._io, self, self._root)
            elif _on == Zip.MagicType.end_of_central_dir:
                self.body = Zip.EndOfCentralDir(self._io, self, self._root)


    class ExtraFieldEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_id = self._io.read_u2le()
            self.data_size = self._io.read_u2le()
            self.data = self._io.read_bytes(self.data_size)


    class Zip64EndOfCentralDirLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number_with_zip64_end = self._io.read_u4le()
            self.end_of_central_dir_offset = self._io.read_u8le()
            self.total_number_of_disks = self._io.read_u4le()


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
            self.num_entries = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.comment_length = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_length)).decode(u"UTF-8")



