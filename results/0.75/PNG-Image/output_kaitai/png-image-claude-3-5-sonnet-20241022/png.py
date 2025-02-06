# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):

    class ColorType(Enum):
        greyscale = 0
        truecolor = 2
        indexed = 3
        greyscale_alpha = 4
        truecolor_alpha = 6

    class InterlaceMethod(Enum):
        none = 0
        adam7 = 1

    class PhysUnit(Enum):
        unknown = 0
        meter = 1

    class SrgbIntent(Enum):
        perceptual = 0
        relative_colorimetric = 1
        saturation = 2
        absolute_colorimetric = 3
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

    class ChunkHistogram(KaitaiStruct):
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



    class ChunkBackground(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            _on = self._root.chunks[0].data.color_type
            if _on == Png.ColorType.indexed:
                self.data = self._io.read_u1()
            elif _on == Png.ColorType.truecolor_alpha:
                self._raw_data = self._io.read_bytes(self._parent.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.TruecolorBackground(_io__raw_data, self, self._root)
            elif _on == Png.ColorType.greyscale_alpha:
                self.data = self._io.read_u2be()
            elif _on == Png.ColorType.truecolor:
                self._raw_data = self._io.read_bytes(self._parent.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.TruecolorBackground(_io__raw_data, self, self._root)
            elif _on == Png.ColorType.greyscale:
                self.data = self._io.read_u2be()
            else:
                self.data = self._io.read_bytes(self._parent.length)


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
            self.type = (self._io.read_bytes(4)).decode(u"ascii")
            _on = self.type
            if _on == u"iTXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkInternationalText(_io__raw_data, self, self._root)
            elif _on == u"gAMA":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkGamma(_io__raw_data, self, self._root)
            elif _on == u"sBIT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkSignificantBits(_io__raw_data, self, self._root)
            elif _on == u"tIME":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkTime(_io__raw_data, self, self._root)
            elif _on == u"tRNS":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkTransparency(_io__raw_data, self, self._root)
            elif _on == u"PLTE":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkPalette(_io__raw_data, self, self._root)
            elif _on == u"bKGD":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkBackground(_io__raw_data, self, self._root)
            elif _on == u"pHYs":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkPhysical(_io__raw_data, self, self._root)
            elif _on == u"sPLT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkSuggestedPalette(_io__raw_data, self, self._root)
            elif _on == u"tEXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkText(_io__raw_data, self, self._root)
            elif _on == u"cHRM":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkChromaticity(_io__raw_data, self, self._root)
            elif _on == u"iCCP":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkIccProfile(_io__raw_data, self, self._root)
            elif _on == u"IHDR":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkHeader(_io__raw_data, self, self._root)
            elif _on == u"hIST":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkHistogram(_io__raw_data, self, self._root)
            elif _on == u"IDAT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkData(_io__raw_data, self, self._root)
            elif _on == u"sRGB":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkStandardRgb(_io__raw_data, self, self._root)
            elif _on == u"zTXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChunkCompressedText(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()


    class ChunkTime(KaitaiStruct):
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


    class ChunkIccProfile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.profile_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.compression_method = self._io.read_u1()
            self.compressed_profile = self._io.read_bytes(((self._parent.length - len(self.profile_name)) - 2))


    class ChunkChromaticity(KaitaiStruct):
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


    class ChunkSignificantBits(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes(self._parent.length)


    class ChunkPhysical(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixels_per_unit_x = self._io.read_u4be()
            self.pixels_per_unit_y = self._io.read_u4be()
            self.unit = KaitaiStream.resolve_enum(Png.PhysUnit, self._io.read_u1())


    class ChunkInternationalText(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.compression_flag = self._io.read_u1()
            self.compression_method = self._io.read_u1()
            self.language_tag = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.translated_keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf8")
            self.text = self._io.read_bytes(((((self._parent.length - len(self.keyword)) - len(self.language_tag)) - len(self.translated_keyword)) - 5))


    class ChunkSuggestedPalette(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.palette_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.sample_depth = self._io.read_u1()
            self.entries = []
            i = 0
            while not self._io.is_eof():
                _on = self.sample_depth
                if _on == 8:
                    self.entries.append(Png.SpltEntry8(self._io, self, self._root))
                elif _on == 16:
                    self.entries.append(Png.SpltEntry16(self._io, self, self._root))
                i += 1



    class ChunkGamma(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gamma_int = self._io.read_u4be()


    class TruecolorBackground(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u2be()
            self.green = self._io.read_u2be()
            self.blue = self._io.read_u2be()


    class ChunkCompressedText(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.compression_method = self._io.read_u1()
            self.compressed_text = self._io.read_bytes(((self._parent.length - len(self.keyword)) - 2))


    class ChunkTransparency(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes(self._parent.length)


    class ChunkText(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.text_data = (self._io.read_bytes(((self._parent.length - len(self.keyword)) - 1))).decode(u"ascii")


    class ChunkPalette(KaitaiStruct):
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



    class ChunkData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes(self._parent.length)


    class SpltEntry16(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u2be()
            self.green = self._io.read_u2be()
            self.blue = self._io.read_u2be()
            self.alpha = self._io.read_u2be()
            self.frequency = self._io.read_u2be()


    class SpltEntry8(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u1()
            self.green = self._io.read_u1()
            self.blue = self._io.read_u1()
            self.alpha = self._io.read_u1()
            self.frequency = self._io.read_u2be()


    class ChunkStandardRgb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.rendering_intent = KaitaiStream.resolve_enum(Png.SrgbIntent, self._io.read_u1())


    class ChunkHeader(KaitaiStruct):
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



