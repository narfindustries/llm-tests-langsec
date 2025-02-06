# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):

    class ColorType(Enum):
        grayscale = 0
        rgb = 2
        palette = 3
        grayscale_alpha = 4
        rgb_alpha = 6

    class InterlaceMethod(Enum):
        none = 0
        adam7 = 1

    class RenderingIntent(Enum):
        perceptual = 0
        relative_colorimetric = 1
        saturation = 2
        absolute_colorimetric = 3

    class PhysUnit(Enum):
        unknown = 0
        meter = 1
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(8)
        if not self.magic == b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A":
            raise kaitaistruct.ValidationNotEqualError(b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", self.magic, self._io, u"/seq/0")
        self.chunks = []
        i = 0
        while True:
            _ = Png.Chunk(self._io, self, self._root)
            self.chunks.append(_)
            if _.type == u"IEND":
                break
            i += 1

    class Rgb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.r = self._io.read_u1()
            self.g = self._io.read_u1()
            self.b = self._io.read_u1()


    class Chunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4be()
            self.type = (self._io.read_bytes(4)).decode(u"ASCII")
            _on = self.type
            if _on == u"iTXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Itxt(_io__raw_data, self, self._root)
            elif _on == u"gAMA":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Gama(_io__raw_data, self, self._root)
            elif _on == u"sBIT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Sbit(_io__raw_data, self, self._root)
            elif _on == u"tIME":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Time(_io__raw_data, self, self._root)
            elif _on == u"tRNS":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Trns(_io__raw_data, self, self._root)
            elif _on == u"PLTE":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Plte(_io__raw_data, self, self._root)
            elif _on == u"bKGD":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Bkgd(_io__raw_data, self, self._root)
            elif _on == u"pHYs":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Phys(_io__raw_data, self, self._root)
            elif _on == u"sPLT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Splt(_io__raw_data, self, self._root)
            elif _on == u"tEXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Text(_io__raw_data, self, self._root)
            elif _on == u"cHRM":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Chrm(_io__raw_data, self, self._root)
            elif _on == u"iCCP":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Iccp(_io__raw_data, self, self._root)
            elif _on == u"IHDR":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Ihdr(_io__raw_data, self, self._root)
            elif _on == u"hIST":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Hist(_io__raw_data, self, self._root)
            elif _on == u"IDAT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Idat(_io__raw_data, self, self._root)
            elif _on == u"sRGB":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Srgb(_io__raw_data, self, self._root)
            elif _on == u"zTXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.Ztxt(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()


    class Ztxt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.compression_method = self._io.read_u1()
            self.compressed_text = self._io.read_bytes_full()


    class Gama(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gamma = self._io.read_u4be()


    class Splt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.sample_depth = self._io.read_u1()
            self.entries = self._io.read_bytes_full()


    class Phys(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixels_per_unit_x = self._io.read_u4be()
            self.pixels_per_unit_y = self._io.read_u4be()
            self.unit = KaitaiStream.resolve_enum(Png.PhysUnit, self._io.read_u1())


    class Idat(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Chrm(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.white_point_x = self._io.read_u4be()
            self.white_point_y = self._io.read_u4be()
            self.red_x = self._io.read_u4be()
            self.red_y = self._io.read_u4be()
            self.green_x = self._io.read_u4be()
            self.green_y = self._io.read_u4be()
            self.blue_x = self._io.read_u4be()
            self.blue_y = self._io.read_u4be()


    class Iccp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.compression_method = self._io.read_u1()
            self.compressed_profile = self._io.read_bytes_full()


    class Text(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.text = (self._io.read_bytes_full()).decode(u"ASCII")


    class Srgb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.rendering_intent = KaitaiStream.resolve_enum(Png.RenderingIntent, self._io.read_u1())


    class Ihdr(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.width = self._io.read_u4be()
            self.height = self._io.read_u4be()
            self.bit_depth = self._io.read_u1()
            self.color_type = KaitaiStream.resolve_enum(Png.ColorType, self._io.read_u1())
            self.compression_method = self._io.read_u1()
            self.filter_method = self._io.read_u1()
            self.interlace_method = KaitaiStream.resolve_enum(Png.InterlaceMethod, self._io.read_u1())


    class Trns(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Itxt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.compression_flag = self._io.read_u1()
            self.compression_method = self._io.read_u1()
            self.language_tag = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.translated_keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")
            self.text = (self._io.read_bytes_full()).decode(u"UTF-8")


    class Time(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.year = self._io.read_u2be()
            self.month = self._io.read_u1()
            self.day = self._io.read_u1()
            self.hour = self._io.read_u1()
            self.minute = self._io.read_u1()
            self.second = self._io.read_u1()


    class Sbit(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Plte(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while not self._io.is_eof():
                self.entries.append(Png.Rgb(self._io, self, self._root))
                i += 1



    class Bkgd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Hist(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.frequencies = []
            i = 0
            while not self._io.is_eof():
                self.frequencies.append(self._io.read_u2be())
                i += 1




