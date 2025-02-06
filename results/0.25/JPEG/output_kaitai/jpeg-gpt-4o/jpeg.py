# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
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
            if _.marker == 65497:
                break
            i += 1

    class DqtSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.info = self._io.read_u1()
            self.quantization_table = []
            for i in range(64):
                self.quantization_table.append(self._io.read_u1())



    class Sof0Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.precision = self._io.read_u1()
            self.height = self._io.read_u2be()
            self.width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.Component(self._io, self, self._root))



    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u2be()
            if  ((self.marker != 65496) and (self.marker != 65497)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != 65496) and (self.marker != 65497)) :
                _on = self.marker
                if _on == 65534:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.ComSegment(_io__raw_data, self, self._root)
                elif _on == 65476:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.DhtSegment(_io__raw_data, self, self._root)
                elif _on == 65498:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.SosSegment(_io__raw_data, self, self._root)
                elif _on == 65472:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.Sof0Segment(_io__raw_data, self, self._root)
                elif _on == 65499:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.DqtSegment(_io__raw_data, self, self._root)
                elif _on == 65501:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.DriSegment(_io__raw_data, self, self._root)
                elif _on == 65504:
                    self._raw_data = self._io.read_bytes((self.length - 2))
                    _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                    self.data = Jpeg.App0Segment(_io__raw_data, self, self._root)
                else:
                    self.data = self._io.read_bytes((self.length - 2))



    class DriSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.restart_interval = self._io.read_u2be()


    class DhtSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.info = self._io.read_u1()
            self.num_codes = []
            for i in range(16):
                self.num_codes.append(self._io.read_u1())

            self.values = []
            i = 0
            while not self._io.is_eof():
                self.values.append(self._io.read_u1())
                i += 1



    class ComSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.comment = (self._io.read_bytes((self._root.segments[i].length - 2))).decode(u"ASCII")


    class SosSegment(KaitaiStruct):
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
            self.approx = self._io.read_u1()


    class App0Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = (self._io.read_bytes(5)).decode(u"ASCII")
            self.version_major = self._io.read_u1()
            self.version_minor = self._io.read_u1()
            self.density_units = self._io.read_u1()
            self.x_density = self._io.read_u2be()
            self.y_density = self._io.read_u2be()
            self.x_thumbnail = self._io.read_u1()
            self.y_thumbnail = self._io.read_u1()
            self.thumbnail_data = self._io.read_bytes(((self.x_thumbnail * self.y_thumbnail) * 3))


    class SosComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u1()
            self.huffman_table = self._io.read_u1()


    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u1()
            self.sampling_factors = self._io.read_u1()
            self.quantization_table_id = self._io.read_u1()



