# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):

    class MessageTypes(Enum):
        adt = 1
        orm = 2
        oru = 3
        rde = 4
        rgv = 5
        mdm = 6
        vxu = 7
        siu = 8

    class TriggerEvents(Enum):
        admit_patient = 1
        transfer_patient = 2
        discharge_patient = 3
        register_patient = 4
        merge_patient = 5
        order_message = 6
        observation = 7
        vaccination = 8

    class DataTypes(Enum):
        string = 1
        numeric = 2
        date = 3
        time = 4
        datetime = 5
        composite_id = 6
        coded_element = 7
        person_name = 8
        address = 9
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.message = []
        i = 0
        while not self._io.is_eof():
            self.message.append(Hl7V2.Hl7Message(self._io, self, self._root))
            i += 1


    class Subcomponent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(38, False, True, True)).decode(u"ASCII")


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.components = []
            i = 0
            while not self._io.is_eof():
                self.components.append(Hl7V2.Component(self._io, self, self._root))
                i += 1



    class Hl7Message(KaitaiStruct):
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
            self.segment_name = (self._io.read_bytes(3)).decode(u"ASCII")
            self.separator = self._io.read_u1()
            if not self.separator == 124:
                raise kaitaistruct.ValidationNotEqualError(124, self.separator, self._io, u"/types/segment/seq/1")
            self.fields = []
            i = 0
            while not self._io.is_eof():
                self.fields.append(Hl7V2.Field(self._io, self, self._root))
                i += 1



    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.subcomponents = []
            i = 0
            while not self._io.is_eof():
                self.subcomponents.append(Hl7V2.Subcomponent(self._io, self, self._root))
                i += 1




