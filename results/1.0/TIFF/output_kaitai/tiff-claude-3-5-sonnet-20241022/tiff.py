# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class Endian(Enum):
        le = 18761
        be = 19789

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

    class TagEnum(Enum):
        subfile_type = 254
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
        resolution_unit = 296
        color_map = 320
        extra_samples = 338
        copyright = 33432
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Tiff.Header(self._io, self, self._root)

    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_dir_entries = self._io.read_u2le()
            self.entries = []
            for i in range(self.num_dir_entries):
                self.entries.append(Tiff.DirectoryEntry(self._io, self, self._root))

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


    class StrAscii(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.str = (KaitaiStream.bytes_terminate(self._io.read_bytes_full(), 0, False)).decode(u"ASCII")


    class DirectoryEntry(KaitaiStruct):
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

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            if self.value_size > 4:
                _pos = self._io.pos()
                self._io.seek(self.value_offset)
                _on = self.field_type
                if _on == Tiff.FieldTypeEnum.undefined:
                    self._m_value = self._io.read_u1()
                elif _on == Tiff.FieldTypeEnum.srational:
                    self._raw__m_value = self._io.read_bytes((self.count * self.type_size))
                    _io__raw__m_value = KaitaiStream(BytesIO(self._raw__m_value))
                    self._m_value = Tiff.Srational(_io__raw__m_value, self, self._root)
                elif _on == Tiff.FieldTypeEnum.rational:
                    self._raw__m_value = self._io.read_bytes((self.count * self.type_size))
                    _io__raw__m_value = KaitaiStream(BytesIO(self._raw__m_value))
                    self._m_value = Tiff.Rational(_io__raw__m_value, self, self._root)
                elif _on == Tiff.FieldTypeEnum.sbyte:
                    self._m_value = self._io.read_s1()
                elif _on == Tiff.FieldTypeEnum.byte:
                    self._m_value = self._io.read_u1()
                elif _on == Tiff.FieldTypeEnum.sshort:
                    self._m_value = self._io.read_s2le()
                elif _on == Tiff.FieldTypeEnum.short:
                    self._m_value = self._io.read_u2le()
                elif _on == Tiff.FieldTypeEnum.long:
                    self._m_value = self._io.read_u4le()
                elif _on == Tiff.FieldTypeEnum.float:
                    self._m_value = self._io.read_f4le()
                elif _on == Tiff.FieldTypeEnum.ascii:
                    self._raw__m_value = self._io.read_bytes((self.count * self.type_size))
                    _io__raw__m_value = KaitaiStream(BytesIO(self._raw__m_value))
                    self._m_value = Tiff.StrAscii(_io__raw__m_value, self, self._root)
                elif _on == Tiff.FieldTypeEnum.slong:
                    self._m_value = self._io.read_s4le()
                elif _on == Tiff.FieldTypeEnum.double:
                    self._m_value = self._io.read_f8le()
                else:
                    self._m_value = self._io.read_bytes((self.count * self.type_size))
                self._io.seek(_pos)

            return getattr(self, '_m_value', None)

        @property
        def type_size(self):
            if hasattr(self, '_m_type_size'):
                return self._m_type_size

            self._m_type_size = (1 if self.field_type == Tiff.FieldTypeEnum.byte else (1 if self.field_type == Tiff.FieldTypeEnum.ascii else (2 if self.field_type == Tiff.FieldTypeEnum.short else (4 if self.field_type == Tiff.FieldTypeEnum.long else (8 if self.field_type == Tiff.FieldTypeEnum.rational else (1 if self.field_type == Tiff.FieldTypeEnum.sbyte else (1 if self.field_type == Tiff.FieldTypeEnum.undefined else (2 if self.field_type == Tiff.FieldTypeEnum.sshort else (4 if self.field_type == Tiff.FieldTypeEnum.slong else (8 if self.field_type == Tiff.FieldTypeEnum.srational else (4 if self.field_type == Tiff.FieldTypeEnum.float else (8 if self.field_type == Tiff.FieldTypeEnum.double else 0))))))))))))
            return getattr(self, '_m_type_size', None)

        @property
        def value_size(self):
            if hasattr(self, '_m_value_size'):
                return self._m_value_size

            self._m_value_size = (self.count * self.type_size)
            return getattr(self, '_m_value_size', None)


    class Srational(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.numerator = self._io.read_s4le()
            self.denominator = self._io.read_s4le()


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
        def first_ifd(self):
            if hasattr(self, '_m_first_ifd'):
                return self._m_first_ifd

            _pos = self._io.pos()
            self._io.seek(self.ifd_offset)
            self._m_first_ifd = Tiff.Ifd(self._io, self, self._root)
            self._io.seek(_pos)
            return getattr(self, '_m_first_ifd', None)


    class Rational(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.numerator = self._io.read_u4le()
            self.denominator = self._io.read_u4le()



