# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):
    """TIFF is a flexible, adaptable file format for handling images and data
    within a single file, by including the header tags (size, definition, image-data arrangement, applied image compression) defining the image's geometry.
    For example, a TIFF file can be a container holding compressed (lossy) JPEG and (lossless) PackBits compressed images.
    TIFF files also can include vector-based clipping paths (outlines, croppings, image frames).
    """

    class TiffByteOrder(Enum):
        le = 18761
        be = 19789

    class TagType(Enum):
        image_width = 256
        image_length = 257
        bits_per_sample = 258
        compression = 259
        photometric_interpretation = 262
        image_description = 270
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

    class TagDataType(Enum):
        byte = 1
        ascii = 2
        short = 3
        long = 4
        rational = 5
        signed_byte = 6
        undefined = 7
        signed_short = 8
        signed_long = 9
        signed_rational = 10
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
            self.byte_order = KaitaiStream.resolve_enum(Tiff.TiffByteOrder, self._io.read_u2le())
            self.magic = self._io.read_bytes(2)
            if not self.magic == b"\x2A\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x2A\x00", self.magic, self._io, u"/types/header/seq/1")
            self.offset_first_ifd = self._io.read_u4le()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_tags = self._io.read_u2le()
            self.tags = []
            for i in range(self.num_tags):
                self.tags.append(Tiff.Tag(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4le()

        @property
        def next_ifd(self):
            if hasattr(self, '_m_next_ifd'):
                return self._m_next_ifd

            if self.next_ifd_offset != 0:
                _pos = self._io.pos()
                self._io.seek(self.next_ifd_offset)
                self._m_next_ifd = Tiff.Ifd(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_next_ifd', None)


    class Tag(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag_type = KaitaiStream.resolve_enum(Tiff.TagType, self._io.read_u2le())
            self.tag_data_type = KaitaiStream.resolve_enum(Tiff.TagDataType, self._io.read_u2le())
            self.num_values = self._io.read_u4le()
            self.value_offset = self._io.read_u4le()



