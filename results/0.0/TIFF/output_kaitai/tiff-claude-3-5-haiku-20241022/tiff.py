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

    class NewSubfileType(Enum):
        reduced_resolution = 1
        page = 2
        transparency_mask = 4

    class Compression(Enum):
        none = 1
        ccitt_group3_fax = 2
        ccitt_group4_fax = 3
        lzw = 4
        jpeg = 5
        jpeg_old = 6
        packbits = 32773

    class PlanarConfiguration(Enum):
        chunky = 1
        planar = 2

    class ResolutionUnit(Enum):
        none = 1
        inches = 2
        centimeters = 3

    class Orientation(Enum):
        top_left = 1
        top_right = 2
        bottom_right = 3
        bottom_left = 4
        left_top = 5
        right_top = 6
        right_bottom = 7
        left_bottom = 8

    class ByteOrder(Enum):
        little_endian = 18761
        big_endian = 19789
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        _on = self.byte_order
        if _on == Tiff.ByteOrder.little_endian:
            self._is_le = True
        elif _on == Tiff.ByteOrder.big_endian:
            self._is_le = False
        if not hasattr(self, '_is_le'):
            raise kaitaistruct.UndecidedEndiannessError("/")
        elif self._is_le == True:
            self._read_le()
        elif self._is_le == False:
            self._read_be()

    def _read_le(self):
        self.byte_order = KaitaiStream.resolve_enum(Tiff.ByteOrder, self._io.read_u2le())
        self.magic_number = self._io.read_bytes(2)
        if not self.magic_number == b"\x2A\x00":
            raise kaitaistruct.ValidationNotEqualError(b"\x2A\x00", self.magic_number, self._io, u"/seq/1")
        self.first_ifd_offset = self._io.read_u4le()
        self.ifd = []
        i = 0
        while True:
            _ = Tiff.ImageFileDirectory(self._io, self, self._root, self._is_le)
            self.ifd.append(_)
            if _.next_ifd_offset == 0:
                break
            i += 1

    def _read_be(self):
        self.byte_order = KaitaiStream.resolve_enum(Tiff.ByteOrder, self._io.read_u2be())
        self.magic_number = self._io.read_bytes(2)
        if not self.magic_number == b"\x2A\x00":
            raise kaitaistruct.ValidationNotEqualError(b"\x2A\x00", self.magic_number, self._io, u"/seq/1")
        self.first_ifd_offset = self._io.read_u4be()
        self.ifd = []
        i = 0
        while True:
            _ = Tiff.ImageFileDirectory(self._io, self, self._root, self._is_le)
            self.ifd.append(_)
            if _.next_ifd_offset == 0:
                break
            i += 1

    class ImageFileDirectory(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/image_file_directory")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.num_fields = self._io.read_u2le()
            self.fields = []
            for i in range(self.num_fields):
                self.fields.append(Tiff.IfdField(self._io, self, self._root, self._is_le))

            self.next_ifd_offset = self._io.read_u4le()

        def _read_be(self):
            self.num_fields = self._io.read_u2be()
            self.fields = []
            for i in range(self.num_fields):
                self.fields.append(Tiff.IfdField(self._io, self, self._root, self._is_le))

            self.next_ifd_offset = self._io.read_u4be()


    class IfdField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/ifd_field")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.tag = self._io.read_u2le()
            self.field_type = self._io.read_u2le()
            self.num_values = self._io.read_u4le()
            self.value_or_offset = self._io.read_u4le()

        def _read_be(self):
            self.tag = self._io.read_u2be()
            self.field_type = self._io.read_u2be()
            self.num_values = self._io.read_u4be()
            self.value_or_offset = self._io.read_u4be()


    class ColorMap(KaitaiStruct):
        def __init__(self, bits_per_sample, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self.bits_per_sample = bits_per_sample
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/color_map")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.red_values = []
            for i in range((1 << self.bits_per_sample)):
                self.red_values.append(self._io.read_u2le())

            self.green_values = []
            for i in range((1 << self.bits_per_sample)):
                self.green_values.append(self._io.read_u2le())

            self.blue_values = []
            for i in range((1 << self.bits_per_sample)):
                self.blue_values.append(self._io.read_u2le())


        def _read_be(self):
            self.red_values = []
            for i in range((1 << self.bits_per_sample)):
                self.red_values.append(self._io.read_u2be())

            self.green_values = []
            for i in range((1 << self.bits_per_sample)):
                self.green_values.append(self._io.read_u2be())

            self.blue_values = []
            for i in range((1 << self.bits_per_sample)):
                self.blue_values.append(self._io.read_u2be())



    class AsciiString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/ascii_string")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.value = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")

        def _read_be(self):
            self.value = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class Rational(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/rational")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.numerator = self._io.read_u4le()
            self.denominator = self._io.read_u4le()

        def _read_be(self):
            self.numerator = self._io.read_u4be()
            self.denominator = self._io.read_u4be()



