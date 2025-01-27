# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2Gpt4Turbo(KaitaiStruct):
    """HL7 (Health Level Seven) is a set of international standards for the exchange, integration, sharing, and retrieval of electronic health information.
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
            self.segments.append(Hl7V2Gpt4Turbo.Segment(self._io, self, self._root))
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
                _ = Hl7V2Gpt4Turbo.Field(self._io, self, self._root)
                self.fields.append(_)
                if _.is_terminator:
                    break
                i += 1


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")

        @property
        def is_terminator(self):
            if hasattr(self, '_m_is_terminator'):
                return self._m_is_terminator

            self._m_is_terminator = self.content == u"\\r"
            return getattr(self, '_m_is_terminator', None)



