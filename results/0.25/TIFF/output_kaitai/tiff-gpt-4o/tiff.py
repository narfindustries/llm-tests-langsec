# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class ByteOrder(Enum):
        le = 18761
        be = 19789

    class TagType(Enum):
        image_width = 256
        image_length = 257
        bits_per_sample = 258
        compression = 259
        photometric_interpretation = 262
        strip_offsets = 273
        samples_per_pixel = 277
        rows_per_strip = 278
        strip_byte_counts = 279
        x_resolution = 282
        y_resolution = 283
        planar_configuration = 284
        resolution_unit = 296
        software = 305
        date_time = 306
        artist = 315
        host_computer = 316
        color_map = 320
        tile_width = 322
        tile_length = 323
        tile_offsets = 324
        tile_byte_counts = 325
        sub_ifds = 330
        jpeg_proc = 512
        jpeg_interchange_format = 513
        jpeg_interchange_format_length = 514
        ycbcr_coefficients = 529
        ycbcr_sub_sampling = 530
        ycbcr_positioning = 531
        reference_black_white = 532

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

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_order = KaitaiStream.resolve_enum(Tiff.ByteOrder, self._io.read_u2be())
            self.version = self._io.read_u2be()
            self.ifd_offset = self._io.read_u4be()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_entries = self._io.read_u2be()
            self.entries = []
            for i in range(self.num_entries):
                self.entries.append(Tiff.IfdEntry(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4be()


    class IfdEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.TagType, self._io.read_u2be())
            self.field_type = KaitaiStream.resolve_enum(Tiff.FieldType, self._io.read_u2be())
            self.num_values = self._io.read_u4be()
            self.value_offset = self._io.read_u4be()



