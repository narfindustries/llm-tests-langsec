# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):

    class MarkerType(Enum):
        sof0 = 192
        sof1 = 193
        sof2 = 194
        sof3 = 195
        dht = 196
        soi = 216
        eoi = 217
        sos = 218
        dqt = 219
        dri = 221
        app0 = 224
        app1 = 225
        app2 = 226
        app3 = 227
        app4 = 228
        app5 = 229
        app6 = 230
        app7 = 231
        app8 = 232
        app9 = 233
        app10 = 234
        app11 = 235
        app12 = 236
        app13 = 237
        app14 = 238
        app15 = 239
        com = 254

    class DensityUnits(Enum):
        none = 0
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
        while True:
            _ = Jpeg.Segment(self._io, self, self._root)
            self.segments.append(_)
            if _.marker == Jpeg.MarkerType.eoi:
                break
            i += 1

    class SegmentSof(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.sample_precision = self._io.read_u1()
            self.height = self._io.read_u2be()
            self.width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.Component(self._io, self, self._root))



    class QuantizationTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_info = self._io.read_u1()
            self.values = []
            for i in range(64):
                _on = self.precision
                if _on == 0:
                    self.values.append(self._io.read_u1())
                elif _on == 1:
                    self.values.append(self._io.read_u2be())


        @property
        def precision(self):
            if hasattr(self, '_m_precision'):
                return self._m_precision

            self._m_precision = ((self.table_info & 240) >> 4)
            return getattr(self, '_m_precision', None)

        @property
        def table_id(self):
            if hasattr(self, '_m_table_id'):
                return self._m_table_id

            self._m_table_id = (self.table_info & 15)
            return getattr(self, '_m_table_id', None)


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(1)
            if not self.magic == b"\xFF":
                raise kaitaistruct.ValidationNotEqualError(b"\xFF", self.magic, self._io, u"/types/segment/seq/0")
            self.marker = KaitaiStream.resolve_enum(Jpeg.MarkerType, self._io.read_u1())
            _on = self.marker
            if _on == Jpeg.MarkerType.dqt:
                self.data = Jpeg.SegmentDqt(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app12:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app3:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app10:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app7:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app6:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.sof0:
                self.data = Jpeg.SegmentSof(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app15:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app9:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app13:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app0:
                self.data = Jpeg.SegmentApp0(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app2:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.sos:
                self.data = Jpeg.SegmentSos(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.soi:
                self.data = Jpeg.SegmentSoi(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.dri:
                self.data = Jpeg.SegmentDri(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app5:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.sof1:
                self.data = Jpeg.SegmentSof(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.eoi:
                self.data = Jpeg.SegmentEoi(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app14:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.com:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.sof3:
                self.data = Jpeg.SegmentSof(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.dht:
                self.data = Jpeg.SegmentDht(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app8:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app1:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app4:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.sof2:
                self.data = Jpeg.SegmentSof(self._io, self, self._root)
            elif _on == Jpeg.MarkerType.app11:
                self.data = Jpeg.SegmentGeneric(self._io, self, self._root)


    class SegmentEoi(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class SegmentSos(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.ScanComponent(self._io, self, self._root))

            self.start_spectral_selection = self._io.read_u1()
            self.end_spectral_selection = self._io.read_u1()
            self.successive_approximation = self._io.read_u1()
            self.image_data = self._io.read_bytes_full()


    class SegmentDri(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.restart_interval = self._io.read_u2be()


    class SegmentDqt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.QuantizationTable(self._io, self, self._root))
                i += 1



    class SegmentSoi(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class HuffmanTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_info = self._io.read_u1()
            self.num_codes = []
            for i in range(16):
                self.num_codes.append(self._io.read_u1())

            self.values = self._io.read_bytes(self.length_of_values)

        @property
        def table_class(self):
            if hasattr(self, '_m_table_class'):
                return self._m_table_class

            self._m_table_class = ((self.table_info & 240) >> 4)
            return getattr(self, '_m_table_class', None)

        @property
        def table_id(self):
            if hasattr(self, '_m_table_id'):
                return self._m_table_id

            self._m_table_id = (self.table_info & 15)
            return getattr(self, '_m_table_id', None)

        @property
        def length_of_values(self):
            if hasattr(self, '_m_length_of_values'):
                return self._m_length_of_values

            self._m_length_of_values = (((((((((((((((self.num_codes[0] + self.num_codes[1]) + self.num_codes[2]) + self.num_codes[3]) + self.num_codes[4]) + self.num_codes[5]) + self.num_codes[6]) + self.num_codes[7]) + self.num_codes[8]) + self.num_codes[9]) + self.num_codes[10]) + self.num_codes[11]) + self.num_codes[12]) + self.num_codes[13]) + self.num_codes[14]) + self.num_codes[15])
            return getattr(self, '_m_length_of_values', None)


    class SegmentApp0(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.identifier = (self._io.read_bytes(5)).decode(u"ASCII")
            self.version = self._io.read_u2be()
            self.units = KaitaiStream.resolve_enum(Jpeg.DensityUnits, self._io.read_u1())
            self.x_density = self._io.read_u2be()
            self.y_density = self._io.read_u2be()
            self.thumbnail_width = self._io.read_u1()
            self.thumbnail_height = self._io.read_u1()
            self.thumbnail_data = self._io.read_bytes(((self.thumbnail_width * self.thumbnail_height) * 3))


    class SegmentDht(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.HuffmanTable(self._io, self, self._root))
                i += 1



    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.sampling_factors = self._io.read_u1()
            self.quantization_table_id = self._io.read_u1()

        @property
        def sampling_factor_h(self):
            if hasattr(self, '_m_sampling_factor_h'):
                return self._m_sampling_factor_h

            self._m_sampling_factor_h = ((self.sampling_factors & 240) >> 4)
            return getattr(self, '_m_sampling_factor_h', None)

        @property
        def sampling_factor_v(self):
            if hasattr(self, '_m_sampling_factor_v'):
                return self._m_sampling_factor_v

            self._m_sampling_factor_v = (self.sampling_factors & 15)
            return getattr(self, '_m_sampling_factor_v', None)


    class ScanComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.huffman_table_ids = self._io.read_u1()

        @property
        def dc_table_id(self):
            if hasattr(self, '_m_dc_table_id'):
                return self._m_dc_table_id

            self._m_dc_table_id = ((self.huffman_table_ids & 240) >> 4)
            return getattr(self, '_m_dc_table_id', None)

        @property
        def ac_table_id(self):
            if hasattr(self, '_m_ac_table_id'):
                return self._m_ac_table_id

            self._m_ac_table_id = (self.huffman_table_ids & 15)
            return getattr(self, '_m_ac_table_id', None)


    class SegmentGeneric(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.data = self._io.read_bytes((self.length - 2))



