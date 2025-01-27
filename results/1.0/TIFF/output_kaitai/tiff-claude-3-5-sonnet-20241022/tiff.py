# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Tiff.Header(self._io, self, self._root)
        self.ifd0 = Tiff.Ifd(self._io, self, self._root)

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.endianness = self._io.read_bytes(2)
            if not self.endianness == b"\x49\x49":
                raise kaitaistruct.ValidationNotEqualError(b"\x49\x49", self.endianness, self._io, u"/types/header/seq/0")
            self.version = self._io.read_bytes(2)
            if not self.version == b"\x2A\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x2A\x00", self.version, self._io, u"/types/header/seq/1")
            self.ifd_offset = self._io.read_u4le()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_entries = self._io.read_u2le()
            self.entries = []
            for i in range(self.num_entries):
                self.entries.append(Tiff.IfdEntry(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4le()


    class IfdEntry(KaitaiStruct):

        class TagEnum(Enum):
            image_width = 256
            image_length = 257
            bits_per_sample = 258
            compression = 259
            photometric_interpretation = 262
            strip_offsets = 273
            samples_per_pixel = 277
            rows_per_strip = 278
            strip_byte_counts = 279

        class FieldTypeEnum(Enum):
            byte = 1
            ascii = 2
            short = 3
            long = 4
            rational = 5
            sbyte = 6
            undefined = 7
            sshort = 8
            slong = 9
            srational = 10
            float = 11
            double = 12
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.IfdEntry.TagEnum, self._io.read_u2le())
            self.field_type = KaitaiStream.resolve_enum(Tiff.IfdEntry.FieldTypeEnum, self._io.read_u2le())
            self.count = self._io.read_u4le()
            self.value_or_offset = self._io.read_u4le()



