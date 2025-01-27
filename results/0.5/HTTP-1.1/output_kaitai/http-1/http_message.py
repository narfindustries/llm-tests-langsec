# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class HttpMessage(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.start_line = HttpMessage.StartLine(self._io, self, self._root)
        self.headers = HttpMessage.Headers(self._io, self, self._root)
        if  ((self.start_line.method == u"POST") or (self.start_line.method == u"PUT") or (self.start_line.method == u"PATCH")) :
            self.body = self._io.read_bytes_full()


    class StartLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.version = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")


    class Headers(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while True:
                _ = HttpMessage.HeaderEntry(self._io, self, self._root)
                self.entries.append(_)
                if _.is_end_of_headers:
                    break
                i += 1


    class HeaderEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"ASCII")
            self.value = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")

        @property
        def is_end_of_headers(self):
            if hasattr(self, '_m_is_end_of_headers'):
                return self._m_is_end_of_headers

            self._m_is_end_of_headers =  ((self.name == u"") and (self.value == u"")) 
            return getattr(self, '_m_is_end_of_headers', None)



