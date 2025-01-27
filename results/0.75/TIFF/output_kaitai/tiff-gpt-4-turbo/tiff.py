# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):
    """TIFF, Tagged Image File Format, is a file format that is widely used
    for storing raster graphics and images as well as vector images.
    """

    class Endian(Enum):
        le = 18761
        be = 19789

    class TagType(Enum):
        image_width = 256
        image_length = 257
        bits_per_sample = 258
        compression = 259
        photometric_interpretation = 262
        image_description = 270
        make = 271
        model = 272
        strip_offsets = 273
        samples_per_pixel = 277
        rows_per_strip = 278
        strip_byte_counts = 279
        x_resolution = 282
        y_resolution = 283
        planar_configuration = 284
        resolution_unit = 296
        software = 305
        datetime = 306
        artist = 315
        jpeg_interchange_format = 513
        jpeg_interchange_format_length = 514
        ycbcr_positioning = 531
        exif_ifd_pointer = 34665
        gps_info_ifd_pointer = 34853

    class FieldType(Enum):
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
        self.header = Tiff.Header(self._io, self, self._root)
        self.ifds = []
        i = 0
        while not self._io.is_eof():
            self.ifds.append(Tiff.Ifd(self._io, self, self._root))
            i += 1


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_order = KaitaiStream.resolve_enum(Tiff.Endian, self._io.read_u2le())
            self.magic = self._io.read_bytes(1)
            if not self.magic == b"\x2A":
                raise kaitaistruct.ValidationNotEqualError(b"\x2A", self.magic, self._io, u"/types/header/seq/1")
            self.offset = self._io.read_u4le()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_of_entries = self._io.read_u2le()
            self.entries = []
            for i in range(self.num_of_entries):
                self.entries.append(Tiff.IfdEntry(self._io, self, self._root))

            self.next_ifd = self._io.read_u4le()


    class IfdEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.TagType, self._io.read_u2le())
            self.type = KaitaiStream.resolve_enum(Tiff.FieldType, self._io.read_u2le())
            self.length = self._io.read_u4le()
            self.offset = self._io.read_u4le()



