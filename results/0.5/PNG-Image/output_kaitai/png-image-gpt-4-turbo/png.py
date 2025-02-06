# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):
    """PNG is a popular image format that supports lossless data compression.
    PNG is designed to work well in online viewing applications, like web browsers,
    and can be fully streamable with a progressive display option.
    PNG files consist of chunks, where each chunk declares its own size,
    enabling the decoder to skip uninteresting (or unknown) chunks effectively.
    """
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
        while not self._io.is_eof():
            self.chunks.append(Png.Chunk(self._io, self, self._root))
            i += 1


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
            if _on == u"gAMA":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.GamaChunk(_io__raw_body, self, self._root)
            elif _on == u"tIME":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.TimeChunk(_io__raw_body, self, self._root)
            elif _on == u"PLTE":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.PlteChunk(_io__raw_body, self, self._root)
            elif _on == u"bKGD":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.BkgdChunk(_io__raw_body, self, self._root)
            elif _on == u"pHYs":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.PhysChunk(_io__raw_body, self, self._root)
            elif _on == u"tEXt":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.TextChunk(_io__raw_body, self, self._root)
            elif _on == u"cHRM":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.ChrmChunk(_io__raw_body, self, self._root)
            elif _on == u"IHDR":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.IhdrChunk(_io__raw_body, self, self._root)
            elif _on == u"IDAT":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.IdatChunk(_io__raw_body, self, self._root)
            elif _on == u"sRGB":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.SrgbChunk(_io__raw_body, self, self._root)
            elif _on == u"IEND":
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = Png.IendChunk(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()


    class IdatChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes(self._parent.length)


    class ChrmChunk(KaitaiStruct):
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


    class IendChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class IhdrChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.width = self._io.read_u4be()
            self.height = self._io.read_u4be()
            self.bit_depth = self._io.read_u1()
            self.color_type = self._io.read_u1()
            self.compression_method = self._io.read_u1()
            self.filter_method = self._io.read_u1()
            self.interlace_method = self._io.read_u1()


    class PlteChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            for i in range(self._parent.length // 3):
                self.entries.append(Png.Rgb(self._io, self, self._root))



    class SrgbChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.rendering_intent = self._io.read_u1()


    class GamaChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gamma = self._io.read_u4be()


    class BkgdChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.background = self._io.read_bytes(self._parent.length)


    class PhysChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixels_per_unit_x = self._io.read_u4be()
            self.pixels_per_unit_y = self._io.read_u4be()
            self.unit = self._io.read_u1()


    class TextChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")
            self.text = (self._io.read_bytes_full()).decode(u"UTF-8")


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



