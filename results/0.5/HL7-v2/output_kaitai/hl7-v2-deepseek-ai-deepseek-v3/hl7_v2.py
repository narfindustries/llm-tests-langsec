# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Hl7V2.Segment(self._io, self, self._root))
            i += 1


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_id = (self._io.read_bytes(3)).decode(u"ASCII")
            self.fields = []
            i = 0
            while True:
                _ = Hl7V2.Field(self._io, self, self._root)
                self.fields.append(_)
                if self._io.is_eof():
                    break
                i += 1


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_value = (KaitaiStream.bytes_terminate(self._io.read_bytes_full(), 124, False)).decode(u"ASCII")
            self.components = []
            i = 0
            while True:
                _ = Hl7V2.Component(self._io, self, self._root)
                self.components.append(_)
                if self._io.is_eof():
                    break
                i += 1


    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.component_value = (KaitaiStream.bytes_terminate(self._io.read_bytes_full(), 94, False)).decode(u"ASCII")
            self.subcomponents = []
            i = 0
            while True:
                _ = Hl7V2.Subcomponent(self._io, self, self._root)
                self.subcomponents.append(_)
                if self._io.is_eof():
                    break
                i += 1


    class Subcomponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subcomponent_value = (KaitaiStream.bytes_terminate(self._io.read_bytes_full(), 38, False)).decode(u"ASCII")



