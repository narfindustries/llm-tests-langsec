# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):

    class ColorTypes(Enum):
        grayscale = 0
        rgb = 2
        palette = 3
        grayscale_alpha = 4
        rgb_alpha = 6

    class RenderingIntents(Enum):
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
        self.signature = self._io.read_bytes(8)
        if not self.signature == b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A":
            raise kaitaistruct.ValidationNotEqualError(b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", self.signature, self._io, u"/seq/0")
        self.chunks = []
        i = 0
        while True:
            _ = Png.Chunk(self._io, self, self._root)
            self.chunks.append(_)
            if _.chunk_type == u"IEND":
                break
            i += 1

    class Chunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4be()
            self.chunk_type = (self._io.read_bytes(4)).decode(u"ASCII")
            _on = self.chunk_type
            if _on == u"iTXt":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.InternationalTextChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"gAMA":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.GammaChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"tIME":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.TimeChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"tRNS":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.TransparencyChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"PLTE":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.PaletteChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"bKGD":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.BackgroundChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"pHYs":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.PhysicalChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"tEXt":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.TextChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"cHRM":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.ChromaticityChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"IHDR":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.HeaderChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"IDAT":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.ImageDataChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"sRGB":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.SrgbChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"zTXt":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.CompressedTextChunk(_io__raw_chunk_data, self, self._root)
            elif _on == u"IEND":
                self._raw_chunk_data = self._io.read_bytes(self.length)
                _io__raw_chunk_data = KaitaiStream(BytesIO(self._raw_chunk_data))
                self.chunk_data = Png.Chunk.EndChunk(_io__raw_chunk_data, self, self._root)
            else:
                self.chunk_data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()

        class Rgb(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.red = self._io.read_u1()
                self.green = self._io.read_u1()
                self.blue = self._io.read_u1()


        class ChromaticityChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.white_x = self._io.read_u4be()
                self.white_y = self._io.read_u4be()
                self.red_x = self._io.read_u4be()
                self.red_y = self._io.read_u4be()
                self.green_x = self._io.read_u4be()
                self.green_y = self._io.read_u4be()
                self.blue_x = self._io.read_u4be()
                self.blue_y = self._io.read_u4be()


        class TransparencyChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.transparency_data = (self._io.read_bytes_full()).decode(u"ASCII")


        class SrgbChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.rendering_intent = KaitaiStream.resolve_enum(Png.RenderingIntents, self._io.read_u1())


        class CompressedTextChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
                self.compression_method = self._io.read_u1()
                self.compressed_text = (self._io.read_bytes_full()).decode(u"ASCII")


        class HeaderChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.width = self._io.read_u4be()
                self.height = self._io.read_u4be()
                self.bit_depth = self._io.read_u1()
                self.color_type = KaitaiStream.resolve_enum(Png.ColorTypes, self._io.read_u1())
                self.compression_method = self._io.read_u1()
                self.filter_method = self._io.read_u1()
                self.interlace_method = self._io.read_u1()


        class GammaChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.gamma_value = self._io.read_u4be()


        class EndChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                pass


        class PhysicalChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.pixels_per_unit_x = self._io.read_u4be()
                self.pixels_per_unit_y = self._io.read_u4be()
                self.unit_specifier = self._io.read_u1()


        class BackgroundChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.background_data = (self._io.read_bytes_full()).decode(u"ASCII")


        class PaletteChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.palette_entries = []
                for i in range(self._parent.length // 3):
                    self.palette_entries.append(Png.Chunk.Rgb(self._io, self, self._root))



        class ImageDataChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.image_data = (self._io.read_bytes_full()).decode(u"ASCII")


        class InternationalTextChunk(KaitaiStruct):
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
                self.text = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")


        class TextChunk(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
                self.text = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


        class TimeChunk(KaitaiStruct):
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




