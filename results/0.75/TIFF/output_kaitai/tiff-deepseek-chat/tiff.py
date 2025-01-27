# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class ByteOrder(Enum):
        little_endian = 18761
        big_endian = 19789

    class TiffVersion(Enum):
        classic = 42
        big_tiff = 43

    class TiffTag(Enum):
        new_subfile_type = 254
        image_width = 256
        image_length = 257
        bits_per_sample = 258
        compression = 259
        photometric_interpretation = 262
        thresholding = 263
        cell_width = 264
        cell_length = 265
        fill_order = 266
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
        page_name = 285
        x_position = 286
        y_position = 287
        free_offsets = 288
        free_byte_counts = 289
        gray_response_unit = 290
        gray_response_curve = 291
        t4_options = 292
        t6_options = 293
        resolution_unit = 296
        page_number = 297
        transfer_function = 301
        software = 305
        datetime = 306
        artist = 315
        host_computer = 316
        predictor = 317
        white_point = 318
        primary_chromaticities = 319
        color_map = 320
        halftone_hints = 321
        tile_width = 322
        tile_length = 323
        tile_offsets = 324
        tile_byte_counts = 325
        subifds = 330
        ink_set = 332
        ink_names = 333
        number_of_inks = 334
        dot_range = 336
        target_printer = 337
        extra_samples = 338
        sample_format = 339
        s_min_sample_value = 340
        s_max_sample_value = 341
        transfer_range = 342
        clip_path = 343
        xclip_path_units = 344
        yclip_path_units = 345
        indexed = 346
        jpeg_tables = 347
        opi_proxy = 351
        jpeg_proc = 512
        jpeg_interchange_format = 513
        jpeg_interchange_format_length = 514
        jpeg_restart_interval = 515
        jpeg_lossless_predictors = 517
        jpeg_point_transforms = 518
        jpeg_q_tables = 519
        jpeg_dc_tables = 520
        jpeg_ac_tables = 521
        ycbcr_coefficients = 529
        ycbcr_sub_sampling = 530
        ycbcr_positioning = 531
        reference_black_white = 532
        xmp = 700
        copyright = 33432

    class TiffFieldType(Enum):
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
        self.header = Tiff.TiffHeader(self._io, self, self._root)
        self.ifds = []
        i = 0
        while not self._io.is_eof():
            self.ifds.append(Tiff.Ifd(self._io, self, self._root))
            i += 1


    class TiffHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_order = KaitaiStream.resolve_enum(Tiff.ByteOrder, self._io.read_u2le())
            self.version = KaitaiStream.resolve_enum(Tiff.TiffVersion, self._io.read_u2le())
            self.first_ifd_offset = self._io.read_u4le()


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
            self.tag = KaitaiStream.resolve_enum(Tiff.TiffTag, self._io.read_u2le())
            self.field_type = KaitaiStream.resolve_enum(Tiff.TiffFieldType, self._io.read_u2le())
            self.count = self._io.read_u4le()
            self.value_offset = self._io.read_u4le()

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.value_offset if self.field_type == Tiff.TiffFieldType.byte else (self.value_offset if self.field_type == Tiff.TiffFieldType.ascii else (self.value_offset if self.field_type == Tiff.TiffFieldType.short else (self.value_offset if self.field_type == Tiff.TiffFieldType.long else (self.value_offset if self.field_type == Tiff.TiffFieldType.rational else (self.value_offset if self.field_type == Tiff.TiffFieldType.sbyte else (self.value_offset if self.field_type == Tiff.TiffFieldType.undefined else (self.value_offset if self.field_type == Tiff.TiffFieldType.sshort else (self.value_offset if self.field_type == Tiff.TiffFieldType.slong else (self.value_offset if self.field_type == Tiff.TiffFieldType.srational else (self.value_offset if self.field_type == Tiff.TiffFieldType.float else (self.value_offset if self.field_type == Tiff.TiffFieldType.double else self.value_offset))))))))))))
            return getattr(self, '_m_value', None)



