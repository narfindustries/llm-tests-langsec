# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class Endians(Enum):
        le = 18761
        be = 19789

    class FieldTypes(Enum):
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

    class Tags(Enum):
        new_subfile_type = 254
        subfile_type = 255
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
        date_time = 306
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
        bad_block_pointer = 326
        bad_block_length = 327
        bad_block_percent = 328
        sub_ifds = 330
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
        x_clip_path_units = 344
        y_clip_path_units = 345
        recorded_indexed = 346
        jpeg_tables = 347
        opi_proxy = 351
        global_parameters = 400
        profile_type = 401
        fax_profile = 402
        coding_methods = 403
        version_year = 404
        mode_number = 405
        decode = 433
        default_image_color = 434
        jpeg_proc = 512
        jpeg_interchange_format = 513
        jpeg_interchange_format_length = 514
        jpeg_restart_interval = 515
        jpeg_lossless_predictors = 517
        jpeg_point_transforms = 518
        jpeg_q_tables = 519
        jpeg_dc_tables = 520
        jpeg_ac_tables = 521
        y_cb_cr_coefficients = 529
        y_cb_cr_sub_sampling = 530
        y_cb_cr_positioning = 531
        reference_black_white = 532
        xml_packet = 700
        image_id = 32781
        wang_annotation = 32932
        page_at_zero = 32934
        t88_options = 32953
        thermal_palette = 32995
        transparency_indicator = 32997
        delay_for_next_image = 32998
        copyright = 33432
        exposure_time = 33434
        f_number = 33437
        photoshop = 34377
        exif_ifd = 34665
        icc_profile = 34675
        gps_ifd = 34853
        interoperability_ifd = 40965
        h_p_private_data = 50255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Tiff.Header(self._io, self, self._root)
        self.ifds = []
        i = 0
        while True:
            _ = Tiff.Ifd(self._io, self, self._root)
            self.ifds.append(_)
            if _.next_ifd_offset == 0:
                break
            i += 1

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.endian = KaitaiStream.resolve_enum(Tiff.Endians, self._io.read_u2le())
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
            self.num_fields = self._io.read_u2le()
            self.fields = []
            for i in range(self.num_fields):
                self.fields.append(Tiff.IfdField(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4le()


    class IfdField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = KaitaiStream.resolve_enum(Tiff.Tags, self._io.read_u2le())
            self.field_type = KaitaiStream.resolve_enum(Tiff.FieldTypes, self._io.read_u2le())
            self.count = self._io.read_u4le()
            self.data_offset = self._io.read_u4le()


    class Rational(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.numerator = self._io.read_u4le()
            self.denominator = self._io.read_u4le()



