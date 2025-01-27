# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http11(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        if not (self.is_response):
            self.request = Http11.RequestMessage(self._io, self, self._root)

        if self.is_response:
            self.response = Http11.ResponseMessage(self._io, self, self._root)


    class RequestMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.version = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")
            self.headers = []
            i = 0
            while not self._io.is_eof():
                self.headers.append(Http11.Header(self._io, self, self._root))
                i += 1

            self.body = Http11.Body(self._io, self, self._root)


    class ResponseMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.status_code = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.status_text = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")
            self.headers = []
            i = 0
            while not self._io.is_eof():
                self.headers.append(Http11.Header(self._io, self, self._root))
                i += 1

            self.body = Http11.Body(self._io, self, self._root)


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"ASCII")
            self.value = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class Body(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"ASCII")


    @property
    def is_response(self):
        """Determines if the message is a response."""
        if hasattr(self, '_m_is_response'):
            return self._m_is_response

        self._m_is_response = True
        return getattr(self, '_m_is_response', None)


