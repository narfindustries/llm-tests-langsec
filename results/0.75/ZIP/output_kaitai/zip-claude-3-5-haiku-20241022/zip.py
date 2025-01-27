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
        self.signatures = Zip.Signatures(self._io, self, self._root)
        self.entries = []
        i = 0
        while not self._io.is_eof():
            self.entries.append(Zip.Entry(self._io, self, self._root))
            i += 1


    class Signatures(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.local_file_header = self._io.read_bytes(4)
            if not self.local_file_header == b"\x50\x4B\x03\x04":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x03\x04", self.local_file_header, self._io, u"/types/signatures/seq/0")
            self.central_directory_header = self._io.read_bytes(4)
            if not self.central_directory_header == b"\x50\x4B\x01\x02":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x01\x02", self.central_directory_header, self._io, u"/types/signatures/seq/1")
            self.end_of_central_directory_record = self._io.read_bytes(4)
            if not self.end_of_central_directory_record == b"\x50\x4B\x05\x06":
                raise kaitaistruct.ValidationNotEqualError(b"\x50\x4B\x05\x06", self.end_of_central_directory_record, self._io, u"/types/signatures/seq/2")


    class Entry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Zip.Entry.LocalFileHeader(self._io, self, self._root)
            self.body = self._io.read_bytes(self.header.compressed_size)
            if self.header.flags.has_data_descriptor:
                self.data_descriptor = Zip.Entry.DataDescriptor(self._io, self, self._root)


        class LocalFileHeader(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.version = self._io.read_u2le()
                self.flags = Zip.Entry.LocalFileHeader.Flags(self._io, self, self._root)
                self.compression_method = self._io.read_u2le()
                self.last_mod_time = self._io.read_u2le()
                self.last_mod_date = self._io.read_u2le()
                self.crc32 = self._io.read_u4le()
                self.compressed_size = self._io.read_u4le()
                self.uncompressed_size = self._io.read_u4le()
                self.filename_length = self._io.read_u2le()
                self.extra_field_length = self._io.read_u2le()
                self.filename = (self._io.read_bytes(self.filename_length)).decode(u"utf-8")
                self.extra_field = self._io.read_bytes(self.extra_field_length)

            class Flags(KaitaiStruct):
                def __init__(self, _io, _parent=None, _root=None):
                    self._io = _io
                    self._parent = _parent
                    self._root = _root if _root else self
                    self._read()

                def _read(self):
                    self.has_data_descriptor = self._io.read_bits_int_be(1) != 0



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




