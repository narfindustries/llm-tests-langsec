# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class Compression(Enum):
        none = 1
        ccitt_group3_1d = 2
        ccitt_group3_2d = 3
        ccitt_group4 = 4
        lzw = 5
        packbits = 32773

    class Endian(Enum):
        little_endian = 18761
        big_endian = 19789

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

    class Photometric(Enum):
        white_is_zero = 0
        black_is_zero = 1
        rgb = 2
        palette_color = 3
        transparency_mask = 4
        ycbcr = 6

    class PlanarConfiguration(Enum):
        chunky = 1
        planar = 2

    class ResolutionUnit(Enum):
        none = 1
        inch = 2
        centimeter = 3

    class FillOrder(Enum):
        msb_to_lsb = 1
        lsb_to_msb = 2

    class Orientation(Enum):
        top_left = 1
        top_right = 2
        bottom_right = 3
        bottom_left = 4
        left_top = 5
        right_top = 6
        right_bottom = 7
        left_bottom = 8

    class TagEnum(Enum):
        image_width = 256
        image_length = 257
        bits_per_sample = 258
        compression = 259
        photometric_interpretation = 262
        threshholding = 263
        cell_width = 264
        cell_length = 265
        fill_order = 266
        document_name = 269
        image_description = 270
        make = 271
        model = 272
        strip_offsets = 273
        orientation = 274
        samples_per_pixel = 277
        rows_per_strip = 278
        strip_byte_counts = 279
        min_sample_value = 280
        max_sample_value = 281
        x_resolution = 282
        y_resolution = 283
        planar_configuration = 284
        gray_response_unit = 290
        gray_response_curve = 291
        group3_options = 292
        group4_options = 293
        resolution_unit = 296
        software = 305
        date_time = 306
        artist = 315
        host_computer = 316
        color_map = 320
        jpeg_proc = 512
        jpeg_interchange_format = 513
        jpeg_interchange_format_length = 514
        jpeg_restart_interval = 515
        jpeg_qtables = 519
        jpeg_dctables = 520
        jpeg_actables = 521
        ycbcr_coefficients = 529
        ycbcr_subsampling = 530
        ycbcr_positioning = 531
        reference_black_white = 532
        copyright = 33432

    class Threshholding(Enum):
        no_dithering = 1
        ordered_dither = 2
        randomized_dither = 3
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
            self.byte_order = KaitaiStream.resolve_enum(Tiff.Endian, self._io.read_u2le())
            self.version = self._io.read_u2le()
            self.ifd_offset = self._io.read_u4le()

        @property
        def is_le(self):
            if hasattr(self, '_m_is_le'):
                return self._m_is_le

            self._m_is_le = self.byte_order == Tiff.Endian.little_endian
            return getattr(self, '_m_is_le', None)


    class IfdEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.TagEnum, self._io.read_u2le())
            self.field_type = KaitaiStream.resolve_enum(Tiff.FieldTypeEnum, self._io.read_u2le())
            self.count = self._io.read_u4le()
            self.value_offset = self._io.read_u4le()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_fields = self._io.read_u2le()
            self.entries = []
            for i in range(self.num_fields):
                self.entries.append(Tiff.IfdEntry(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4le()



