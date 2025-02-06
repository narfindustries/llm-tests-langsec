# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):

    class MarkerType(Enum):
        sof0 = 65472
        sof1 = 65473
        sof2 = 65474
        sof3 = 65475
        dht = 65476
        rst0 = 65488
        rst1 = 65489
        rst2 = 65490
        rst3 = 65491
        rst4 = 65492
        rst5 = 65493
        rst6 = 65494
        rst7 = 65495
        soi = 65496
        eoi = 65497
        sos = 65498
        dqt = 65499
        dri = 65501
        app0 = 65504
        app1 = 65505
        app2 = 65506
        app3 = 65507
        app4 = 65508
        app5 = 65509
        app6 = 65510
        app7 = 65511
        app8 = 65512
        app9 = 65513
        app10 = 65514
        app11 = 65515
        app12 = 65516
        app13 = 65517
        app14 = 65518
        app15 = 65519
        com = 65534
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



    class RestartInterval(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.interval = self._io.read_u2be()


    class AppData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes((self._parent.length - 2))


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = KaitaiStream.resolve_enum(Jpeg.MarkerType, self._io.read_u2be())
            if  ((self.marker != Jpeg.MarkerType.soi) and (self.marker != Jpeg.MarkerType.eoi) and (self.marker != Jpeg.MarkerType.rst0) and (self.marker != Jpeg.MarkerType.rst1) and (self.marker != Jpeg.MarkerType.rst2) and (self.marker != Jpeg.MarkerType.rst3) and (self.marker != Jpeg.MarkerType.rst4) and (self.marker != Jpeg.MarkerType.rst5) and (self.marker != Jpeg.MarkerType.rst6) and (self.marker != Jpeg.MarkerType.rst7)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != Jpeg.MarkerType.soi) and (self.marker != Jpeg.MarkerType.eoi) and (self.marker != Jpeg.MarkerType.rst0) and (self.marker != Jpeg.MarkerType.rst1) and (self.marker != Jpeg.MarkerType.rst2) and (self.marker != Jpeg.MarkerType.rst3) and (self.marker != Jpeg.MarkerType.rst4) and (self.marker != Jpeg.MarkerType.rst5) and (self.marker != Jpeg.MarkerType.rst6) and (self.marker != Jpeg.MarkerType.rst7)) :
                _on = self.marker
                if _on == Jpeg.MarkerType.dqt:
                    self.data = Jpeg.QuantizationTable(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app12:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app3:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app10:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app7:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app6:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.sof0:
                    self.data = Jpeg.FrameHeader(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app15:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app9:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app13:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app0:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app2:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.sos:
                    self.data = Jpeg.ScanHeader(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.dri:
                    self.data = Jpeg.RestartInterval(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app5:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.sof1:
                    self.data = Jpeg.FrameHeader(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app14:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.com:
                    self.data = Jpeg.CommentData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.sof3:
                    self.data = Jpeg.FrameHeader(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.dht:
                    self.data = Jpeg.HuffmanTable(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app8:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app1:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app4:
                    self.data = Jpeg.AppData(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.sof2:
                    self.data = Jpeg.FrameHeader(self._io, self, self._root)
                elif _on == Jpeg.MarkerType.app11:
                    self.data = Jpeg.AppData(self._io, self, self._root)

            if self.marker == Jpeg.MarkerType.sos:
                self._raw_image_data = self._io.read_bytes_full()
                _io__raw_image_data = KaitaiStream(BytesIO(self._raw_image_data))
                self.image_data = Jpeg.EntropyCodedData(_io__raw_image_data, self, self._root)



    class ScanHeader(KaitaiStruct):
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

            self.start_spectral_selection = self._io.read_u1()
            self.end_spectral_selection = self._io.read_u1()
            self.successive_approximation = self._io.read_u1()


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
            self.num_codes_per_length = []
            for i in range(16):
                self.num_codes_per_length.append(self._io.read_u1())

            self.values = self._io.read_bytes(self.num_codes_length)

        @property
        def num_codes_length(self):
            if hasattr(self, '_m_num_codes_length'):
                return self._m_num_codes_length

            self._m_num_codes_length = (((((((((((((((self.num_codes_per_length[0] + self.num_codes_per_length[1]) + self.num_codes_per_length[2]) + self.num_codes_per_length[3]) + self.num_codes_per_length[4]) + self.num_codes_per_length[5]) + self.num_codes_per_length[6]) + self.num_codes_per_length[7]) + self.num_codes_per_length[8]) + self.num_codes_per_length[9]) + self.num_codes_per_length[10]) + self.num_codes_per_length[11]) + self.num_codes_per_length[12]) + self.num_codes_per_length[13]) + self.num_codes_per_length[14]) + self.num_codes_per_length[15])
            return getattr(self, '_m_num_codes_length', None)


    class EntropyCodedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class CommentData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.comment = (self._io.read_bytes((self._parent.length - 2))).decode(u"ASCII")


    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.sampling_factors = self._io.read_u1()
            self.quantization_table_num = self._io.read_u1()


    class ScanComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.dc_ac_table_selector = self._io.read_u1()



