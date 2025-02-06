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
        ppmd = 98
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.sections = []
        i = 0
        while not self._io.is_eof():
            self.sections.append(Zip.PkSection(self._io, self, self._root))
            i += 1


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



    class PkSectionZip64EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.record_size = self._io.read_u8le()
            self.version_made_by = self._io.read_u2le()
            self.version_needed = self._io.read_u2le()
            self.disk_number = self._io.read_u4le()
            self.disk_start = self._io.read_u4le()
            self.num_entries_this_disk = self._io.read_u8le()
            self.num_entries = self._io.read_u8le()
            self.central_dir_size = self._io.read_u8le()
            self.central_dir_offset = self._io.read_u8le()
            self.extensible_data = self._io.read_bytes((self.record_size - 44))


    class PkSectionZip64EndOfCentralDirLocator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number_with_zip64_end_cd = self._io.read_u4le()
            self.zip64_end_of_central_dir_offset = self._io.read_u8le()
            self.total_number_of_disks = self._io.read_u4le()


    class PkSection(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(2)
            if not self.magic == b"\x50\x4B":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B", self.magic, self._io, u"/types/pk_section/seq/0")
            self.section_type = self._io.read_u2le()
            _on = self.section_type
            if _on == 1027:
                self.body = Zip.PkSectionLocalFile(self._io, self, self._root)
            elif _on == 513:
                self.body = Zip.PkSectionCentralDir(self._io, self, self._root)
            elif _on == 1798:
                self.body = Zip.PkSectionZip64EndOfCentralDirLocator(self._io, self, self._root)
            elif _on == 1541:
                self.body = Zip.PkSectionEndOfCentralDir(self._io, self, self._root)
            elif _on == 1542:
                self.body = Zip.PkSectionZip64EndOfCentralDir(self._io, self, self._root)


    class ExtraFieldEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.code = self._io.read_u2le()
            self.size = self._io.read_u2le()
            self.body = self._io.read_bytes(self.size)


    class PkSectionEndOfCentralDir(KaitaiStruct):
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
            self.comment_len = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")


    class PkSectionLocalFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_needed = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.Compression, self._io.read_u2le())
            self.file_mod_time = self._io.read_u2le()
            self.file_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_len = self._io.read_u2le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self._raw_extra = self._io.read_bytes(self.extra_len)
            _io__raw_extra = KaitaiStream(BytesIO(self._raw_extra))
            self.extra = Zip.ExtraField(_io__raw_extra, self, self._root)
            self.body = self._io.read_bytes(self.compressed_size)


    class PkSectionCentralDir(KaitaiStruct):
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
            self.file_mod_time = self._io.read_u2le()
            self.file_mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.file_name_len = self._io.read_u2le()
            self.extra_len = self._io.read_u2le()
            self.comment_len = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_attrs = self._io.read_u2le()
            self.external_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.file_name = (self._io.read_bytes(self.file_name_len)).decode(u"UTF-8")
            self._raw_extra = self._io.read_bytes(self.extra_len)
            _io__raw_extra = KaitaiStream(BytesIO(self._raw_extra))
            self.extra = Zip.ExtraField(_io__raw_extra, self, self._root)
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"UTF-8")



