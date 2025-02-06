# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
    """JPEG (Joint Photographic Experts Group) image, a very popular image file format
    that uses lossy compression.
    """

    class Markers(Enum):
        sof0 = 65472
        dht = 65476
        soi = 65496
        eoi = 65497
        sos = 65498
        dqt = 65499
        app0 = 65504
        com = 65534

    class DensityUnits(Enum):
        no_units = 0
        pixels_per_inch = 1
        pixels_per_cm = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Jpeg.Segment(self._io, self, self._root))
            i += 1


    class App0(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(5)
            if not self.magic == b"\x4A\x46\x49\x46\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x4A\x46\x49\x46\x00", self.magic, self._io, u"/types/app0/seq/0")
            self.version_major = self._io.read_u1()
            self.version_minor = self._io.read_u1()
            self.density_units = KaitaiStream.resolve_enum(Jpeg.DensityUnits, self._io.read_u1())
            self.density_x = self._io.read_u2be()
            self.density_y = self._io.read_u2be()
            self.thumbnail_x = self._io.read_u1()
            self.thumbnail_y = self._io.read_u1()
            self.thumbnail = self._io.read_bytes(((self.thumbnail_x * self.thumbnail_y) * 3))


    class QuantizationTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_info = self._io.read_bits_int_be(1) != 0
            self._io.align_to_byte()
            self.table = []
            for i in range(64):
                self.table.append(self._io.read_u1())



    class Dht(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.HuffmanTable(self._io, self, self._root))
                i += 1



    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = KaitaiStream.resolve_enum(Jpeg.Markers, self._io.read_u2be())
            if  ((self.marker != Jpeg.Markers.soi) and (self.marker != Jpeg.Markers.eoi)) :
                self.length = self._io.read_u2be()

            _on = self.marker
            if _on == Jpeg.Markers.dqt:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.Dqt(_io__raw_data, self, self._root)
            elif _on == Jpeg.Markers.sos:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.Sos(_io__raw_data, self, self._root)
            elif _on == Jpeg.Markers.com:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.Com(_io__raw_data, self, self._root)
            elif _on == Jpeg.Markers.sof0:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.Sof0(_io__raw_data, self, self._root)
            elif _on == Jpeg.Markers.app0:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.App0(_io__raw_data, self, self._root)
            elif _on == Jpeg.Markers.dht:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.Dht(_io__raw_data, self, self._root)
            else:
                self._raw_data = self._io.read_bytes((self.length - 2))
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Jpeg.UnknownData(_io__raw_data, self, self._root)


    class Sof0(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.precision = self._io.read_u1()
            self.image_height = self._io.read_u2be()
            self.image_width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.Component(self._io, self, self._root))



    class HuffmanTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_info = self._io.read_bits_int_be(1) != 0
            self._io.align_to_byte()
            self.lengths = []
            for i in range(16):
                self.lengths.append(self._io.read_u1())

            self.huffman_values = []
            i = 0
            while not self._io.is_eof():
                self.huffman_values.append(self._io.read_u1())
                i += 1



    class SosComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.huffman_table_id = self._io.read_bits_int_be(1) != 0


    class Sos(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.SosComponent(self._io, self, self._root))

            self.start_spectral = self._io.read_u1()
            self.end_spectral = self._io.read_u1()
            self.approx_high = self._io.read_u1()
            self.approx_low = self._io.read_u1()


    class UnknownData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class Com(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.comment = (self._io.read_bytes_full()).decode(u"ASCII")


    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.sampling_factors = self._io.read_bits_int_be(1) != 0
            self._io.align_to_byte()
            self.quantization_table_id = self._io.read_u1()


    class Dqt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.QuantizationTable(self._io, self, self._root))
                i += 1




