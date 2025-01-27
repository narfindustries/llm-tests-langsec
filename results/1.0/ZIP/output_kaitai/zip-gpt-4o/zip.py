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
        tokenized = 7
        deflated = 8
        enhanced_deflated = 9
        pkware_dcl_imploded = 10
        bzip2 = 12
        lzma = 14
        ibm_terse = 18
        ibm_lz77_z = 19
        wavpack = 97
        ppmd = 98

    class Extras(Enum):
        zip64 = 1
        av_info = 7
        os2 = 9
        ntfs = 10
        openvms = 12
        unix = 13
        reserved_for_futures = 14
        reserved_future = 15
        patched_data = 20
        ms_packaged = 21
        ibm_s390_unix = 101
        ibm_s390_windows = 102
        poszip_4690 = 18064
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
            if _.magic == 101010256:
                break
            i += 1

    class Section(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_u4le()
            _on = self.magic
            if _on == 67324752:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.PkSection(_io__raw_body, self, self._root)
            elif _on == 33639248:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.CentralDirEntry(_io__raw_body, self, self._root)
            elif _on == 101010256:
                self._raw_body = self._io.read_bytes_full()
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Zip.EndOfCentralDir(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes_full()


    class PkSection(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.Compression, self._io.read_u2le())
            self.mod_time = self._io.read_u2le()
            self.mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_len = self._io.read_u2le()
            self.extra_len = self._io.read_u2le()
            self.filename = (self._io.read_bytes(self.filename_len)).decode(u"utf-8")
            self.extra = self._io.read_bytes(self.extra_len)


    class CentralDirEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version_made_by = self._io.read_u2le()
            self.version_needed_to_extract = self._io.read_u2le()
            self.flags = self._io.read_u2le()
            self.compression_method = KaitaiStream.resolve_enum(Zip.Compression, self._io.read_u2le())
            self.mod_time = self._io.read_u2le()
            self.mod_date = self._io.read_u2le()
            self.crc32 = self._io.read_u4le()
            self.compressed_size = self._io.read_u4le()
            self.uncompressed_size = self._io.read_u4le()
            self.filename_len = self._io.read_u2le()
            self.extra_len = self._io.read_u2le()
            self.comment_len = self._io.read_u2le()
            self.disk_number_start = self._io.read_u2le()
            self.internal_file_attrs = self._io.read_u2le()
            self.external_file_attrs = self._io.read_u4le()
            self.local_header_offset = self._io.read_u4le()
            self.filename = (self._io.read_bytes(self.filename_len)).decode(u"utf-8")
            self.extra = self._io.read_bytes(self.extra_len)
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"utf-8")


    class EndOfCentralDir(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.disk_number = self._io.read_u2le()
            self.central_dir_disk_number = self._io.read_u2le()
            self.num_central_dir_entries_disk = self._io.read_u2le()
            self.total_central_dir_entries = self._io.read_u2le()
            self.central_dir_size = self._io.read_u4le()
            self.central_dir_offset = self._io.read_u4le()
            self.comment_len = self._io.read_u2le()
            self.comment = (self._io.read_bytes(self.comment_len)).decode(u"utf-8")



