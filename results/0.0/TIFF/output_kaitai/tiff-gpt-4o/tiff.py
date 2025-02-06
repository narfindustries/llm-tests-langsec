# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class PhotometricInterpretation(Enum):
        white_is_zero = 0
        black_is_zero = 1
        rgb = 2
        palette_color = 3
        transparency_mask = 4
        cmyk = 5
        ycbcr = 6
        cielab = 8

    class Tag(Enum):
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
        color_map = 320
        tile_width = 322
        tile_length = 323
        tile_offsets = 324
        tile_byte_counts = 325
        extra_samples = 338
        sample_format = 339

    class SampleFormat(Enum):
        unsigned_integer = 1
        signed_integer = 2
        ieee_floating_point = 3
        undefined = 4

    class Compression(Enum):
        no_compression = 1
        ccitt_huffman = 2
        ccitt_t4 = 3
        ccitt_t6 = 4
        lzw = 5
        jpeg = 6
        packbits = 32773

    class ExtraSamples(Enum):
        unspecified = 0
        associated_alpha = 1
        unassociated_alpha = 2

    class PlanarConfiguration(Enum):
        chunky = 1
        planar = 2

    class ResolutionUnit(Enum):
        no_absolute_unit = 1
        inch = 2
        centimeter = 3

    class ByteOrder(Enum):
        intel = 18761
        motorola = 19789
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
            self.byte_order = KaitaiStream.resolve_enum(Tiff.ByteOrder, self._io.read_u2le())
            self.version = self._io.read_u2le()
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
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.Tag, self._io.read_u2le())
            self.field_type = self._io.read_u2le()
            self.num_values = self._io.read_u4le()
            self.value_offset = self._io.read_u4le()



