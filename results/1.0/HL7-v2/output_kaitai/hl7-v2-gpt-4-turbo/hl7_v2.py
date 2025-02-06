# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
    """HL7 V2.x messaging is used in the healthcare industry to communicate standard information
    between devices. These messages are highly structured with segments such as MSH (Message Header),
    PID (Patient Identification), etc.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        pass

    class Message(KaitaiStruct):
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
            self.seg_id = (self._io.read_bytes(3)).decode(u"ASCII")
            self.fields = []
            i = 0
            while True:
                _ = Hl7V2.Field(self._io, self, self._root)
                self.fields.append(_)
                if _.field_data == u"|":
                    break
                i += 1


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_data = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class Strz(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.string = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")

        @property
        def eof(self):
            if hasattr(self, '_m_eof'):
                return self._m_eof

            self._m_eof = self._io.pos() == self._io.size()
            return getattr(self, '_m_eof', None)



