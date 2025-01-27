# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
    """Health Level 7 (HL7) is a set of international standards for transfer of clinical and administrative data between software applications used by various healthcare providers.
    """
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
            self.segment_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.fields = []
            i = 0
            while True:
                _ = Hl7V2.Field(self._io, self, self._root)
                self.fields.append(_)
                if _.is_last_field:
                    break
                i += 1


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_content = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")

        @property
        def is_last_field(self):
            if hasattr(self, '_m_is_last_field'):
                return self._m_is_last_field

            self._m_is_last_field = self.field_content == u"\\r"
            return getattr(self, '_m_is_last_field', None)



