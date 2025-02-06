# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):
    """PNG is a bitmap image format that employs lossless data compression.
    PNG was created to improve upon and replace GIF as an image-file format
    not requiring a patent license.
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


    class ZtxtChunk(KaitaiStruct):
        """Compressed textual data."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


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
                self.data = Png.ItxtChunk(_io__raw_data, self, self._root)
            elif _on == u"gAMA":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.GamaChunk(_io__raw_data, self, self._root)
            elif _on == u"sBIT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.SbitChunk(_io__raw_data, self, self._root)
            elif _on == u"tIME":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.TimeChunk(_io__raw_data, self, self._root)
            elif _on == u"tRNS":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.TrnsChunk(_io__raw_data, self, self._root)
            elif _on == u"PLTE":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.PlteChunk(_io__raw_data, self, self._root)
            elif _on == u"bKGD":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.BkgdChunk(_io__raw_data, self, self._root)
            elif _on == u"pHYs":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.PhysChunk(_io__raw_data, self, self._root)
            elif _on == u"tEXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.TextChunk(_io__raw_data, self, self._root)
            elif _on == u"cHRM":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ChrmChunk(_io__raw_data, self, self._root)
            elif _on == u"IHDR":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.IhdrChunk(_io__raw_data, self, self._root)
            elif _on == u"hIST":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.HistChunk(_io__raw_data, self, self._root)
            elif _on == u"IDAT":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.IdatChunk(_io__raw_data, self, self._root)
            elif _on == u"sRGB":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.SrgbChunk(_io__raw_data, self, self._root)
            elif _on == u"zTXt":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.ZtxtChunk(_io__raw_data, self, self._root)
            elif _on == u"IEND":
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Png.IendChunk(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()


    class IdatChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class ChrmChunk(KaitaiStruct):
        """Primary chromaticities and white point."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class IendChunk(KaitaiStruct):
        """Marks the end of the PNG datastream."""
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
        """Standard RGB color space."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class ItxtChunk(KaitaiStruct):
        """Compressed international textual data."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class GamaChunk(KaitaiStruct):
        """Image gamma information."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class TrnsChunk(KaitaiStruct):
        """Transparency information."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class SbitChunk(KaitaiStruct):
        """Significant bits."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class BkgdChunk(KaitaiStruct):
        """Background color."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class PhysChunk(KaitaiStruct):
        """Physical pixel dimensions."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class HistChunk(KaitaiStruct):
        """Image histogram."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class TextChunk(KaitaiStruct):
        """Uncompressed international textual data."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class TimeChunk(KaitaiStruct):
        """Image last-modification time."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass



