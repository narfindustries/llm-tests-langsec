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
        self.start_of_image = self._io.read_bytes(2)
        if not self.start_of_image == b"\xFF\xD8":
            raise kaitaistruct.ValidationNotEqualError(b"\xFF\xD8", self.start_of_image, self._io, u"/seq/0")
        self.segments = []
        i = 0
        while True:
            _ = Jpeg.Segment(self._io, self, self._root)
            self.segments.append(_)
            if _.marker == 65497:
                break
            i += 1

    class DhtTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.table_class = self._io.read_bits_int_be(4)
            self.table_destination = self._io.read_bits_int_be(4)
            self._io.align_to_byte()
            self.bit_lengths = []
            for i in range(16):
                self.bit_lengths.append(self._io.read_u1())

            self.huffman_values = []
            i = 0
            while not self._io.is_eof():
                self.huffman_values.append(self._io.read_u1())
                i += 1



    class UnknownSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.data = self._io.read_bytes((self.length - 2))


    class SofComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.sampling_factors = self._io.read_u1()
            self.quantization_table_id = self._io.read_u1()


    class CommentSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.comment = (self._io.read_bytes((self.length - 2))).decode(u"ASCII")


    class DqtSegment(KaitaiStruct):
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
                self.tables.append(Jpeg.DqtTable(self._io, self, self._root))
                i += 1



    class EndOfImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_bytes(2)
            if not self.marker == b"\xFF\xD9":
                raise kaitaistruct.ValidationNotEqualError(b"\xFF\xD9", self.marker, self._io, u"/types/end_of_image_segment/seq/0")


    class SofProgressiveSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.precision = self._io.read_u1()
            self.height = self._io.read_u2be()
            self.width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.SofComponent(self._io, self, self._root))



    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u2be()
            _on = self.marker
            if _on == 65534:
                self.content = Jpeg.CommentSegment(self._io, self, self._root)
            elif _on == 65493:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65476:
                self.content = Jpeg.DhtSegment(self._io, self, self._root)
            elif _on == 65474:
                self.content = Jpeg.SofProgressiveSegment(self._io, self, self._root)
            elif _on == 65498:
                self.content = Jpeg.SosSegment(self._io, self, self._root)
            elif _on == 65489:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65497:
                self.content = Jpeg.EndOfImageSegment(self._io, self, self._root)
            elif _on == 65472:
                self.content = Jpeg.SofSegment(self._io, self, self._root)
            elif _on == 65499:
                self.content = Jpeg.DqtSegment(self._io, self, self._root)
            elif _on == 65490:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65492:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65488:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65494:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65495:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            elif _on == 65504:
                self.content = Jpeg.App0Segment(self._io, self, self._root)
            elif _on == 65491:
                self.content = Jpeg.RestartMarker(self._io, self, self._root)
            else:
                self.content = Jpeg.UnknownSegment(self._io, self, self._root)


    class DhtSegment(KaitaiStruct):
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
                self.tables.append(Jpeg.DhtTable(self._io, self, self._root))
                i += 1



    class RestartMarker(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u2be()


    class Version(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.major = self._io.read_u1()
            self.minor = self._io.read_u1()


    class SosSegment(KaitaiStruct):
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
                self.components.append(Jpeg.SosComponent(self._io, self, self._root))

            self.start_of_spectral_selection = self._io.read_u1()
            self.end_of_spectral_selection = self._io.read_u1()
            self.approximation_bit = self._io.read_u1()


    class App0Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.identifier = (self._io.read_bytes(5)).decode(u"ASCII")
            self.version = Jpeg.Version(self._io, self, self._root)
            self.density_units = self._io.read_u1()
            self.x_density = self._io.read_u2be()
            self.y_density = self._io.read_u2be()
            self.thumbnail_width = self._io.read_u1()
            self.thumbnail_height = self._io.read_u1()


    class SofSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.precision = self._io.read_u1()
            self.height = self._io.read_u2be()
            self.width = self._io.read_u2be()
            self.num_components = self._io.read_u1()
            self.components = []
            for i in range(self.num_components):
                self.components.append(Jpeg.SofComponent(self._io, self, self._root))



    class SosComponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_id = self._io.read_u1()
            self.dc_ac_table_selectors = self._io.read_u1()


    class DqtTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.precision = self._io.read_bits_int_be(4)
            self.table_id = self._io.read_bits_int_be(4)
            self._io.align_to_byte()
            self.table_data = []
            for i in range(64):
                self.table_data.append(self._io.read_u1())




