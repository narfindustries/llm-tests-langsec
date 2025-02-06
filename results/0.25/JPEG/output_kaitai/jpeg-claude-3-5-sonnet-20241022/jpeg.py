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
        rst0 = 208
        rst1 = 209
        rst2 = 210
        rst3 = 211
        rst4 = 212
        rst5 = 213
        rst6 = 214
        rst7 = 215
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

    class FrameHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sample_precision = self._io.read_u1()
            self.height = self._io.read_u2be()
            self.width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.Component(self._io, self, self._root))



    class QuantizationTableSpec(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.precision_and_id = self._io.read_u1()
            if (self.precision_and_id & 240) == 0:
                self.values = []
                for i in range(64):
                    self.values.append(self._io.read_u1())


            if (self.precision_and_id & 240) != 0:
                self.values_16bit = []
                for i in range(64):
                    self.values_16bit.append(self._io.read_u2be())




    class QuantizationTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.QuantizationTableSpec(self._io, self, self._root))
                i += 1



    class RawData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class RestartInterval(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.interval = self._io.read_u2be()


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
            if  ((self.marker != Jpeg.MarkerType.soi) and (self.marker != Jpeg.MarkerType.eoi) and (self.marker != Jpeg.MarkerType.rst0) and (self.marker != Jpeg.MarkerType.rst1) and (self.marker != Jpeg.MarkerType.rst2) and (self.marker != Jpeg.MarkerType.rst3) and (self.marker != Jpeg.MarkerType.rst4) and (self.marker != Jpeg.MarkerType.rst5) and (self.marker != Jpeg.MarkerType.rst6) and (self.marker != Jpeg.MarkerType.rst7)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != Jpeg.MarkerType.soi) and (self.marker != Jpeg.MarkerType.eoi) and (self.marker != Jpeg.MarkerType.rst0) and (self.marker != Jpeg.MarkerType.rst1) and (self.marker != Jpeg.MarkerType.rst2) and (self.marker != Jpeg.MarkerType.rst3) and (self.marker != Jpeg.MarkerType.rst4) and (self.marker != Jpeg.MarkerType.rst5) and (self.marker != Jpeg.MarkerType.rst6) and (self.marker != Jpeg.MarkerType.rst7)) :
                _on = self.marker
                if _on == Jpeg.MarkerType.dqt:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.QuantizationTable(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.sof0:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.FrameHeader(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.app0:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.App0Segment(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.sos:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.StartOfScan(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.dri:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.RestartInterval(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.sof1:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.FrameHeader(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.sof3:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.FrameHeader(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.dht:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.HuffmanTable(_io__raw_data, self, self._root)
                elif _on == Jpeg.MarkerType.sof2:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.FrameHeader(_io__raw_data, self, self._root)
                else:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.RawData(_io__raw_data, self, self._root)



    class StartOfScan(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.ScanComponent(self._io, self, self._root))

            self.start_spectral = self._io.read_u1()
            self.end_spectral = self._io.read_u1()
            self.approx_high_low = self._io.read_u1()
            self.image_data = self._io.read_bytes_full()


    class HuffmanTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tables = []
            i = 0
            while not self._io.is_eof():
                self.tables.append(Jpeg.HuffmanTableSpec(self._io, self, self._root))
                i += 1



    class HuffmanTableSpec(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_class_and_id = self._io.read_u1()
            self.code_lengths = []
            for i in range(16):
                self.code_lengths.append(self._io.read_u1())

            self.values = []
            for i in range((((((((((((((((self.code_lengths[0] + self.code_lengths[1]) + self.code_lengths[2]) + self.code_lengths[3]) + self.code_lengths[4]) + self.code_lengths[5]) + self.code_lengths[6]) + self.code_lengths[7]) + self.code_lengths[8]) + self.code_lengths[9]) + self.code_lengths[10]) + self.code_lengths[11]) + self.code_lengths[12]) + self.code_lengths[13]) + self.code_lengths[14]) + self.code_lengths[15])):
                self.values.append(self._io.read_u1())



    class App0Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = (self._io.read_bytes(5)).decode(u"ASCII")
            self.version = self._io.read_u2be()
            self.units = self._io.read_u1()
            self.x_density = self._io.read_u2be()
            self.y_density = self._io.read_u2be()
            self.thumbnail_width = self._io.read_u1()
            self.thumbnail_height = self._io.read_u1()
            if  ((self.thumbnail_width != 0) and (self.thumbnail_height != 0)) :
                self.thumbnail_data = self._io.read_bytes(((self.thumbnail_width * self.thumbnail_height) * 3))



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


    class ScanComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.dc_ac_table_ids = self._io.read_u1()



