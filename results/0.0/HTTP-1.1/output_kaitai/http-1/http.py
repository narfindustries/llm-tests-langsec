# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http(KaitaiStruct):
    """HTTP 1.1 (Hypertext Transfer Protocol) is a stateless protocol used primarily for transferring
    web documents (HTML, images, etc.). This specification covers parsing of HTTP request and response
    messages.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.request = Http.HttpRequest(self._io, self, self._root)
        self.response = Http.HttpResponse(self._io, self, self._root)

    class HttpRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.version = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")
            self.headers = []
            i = 0
            while not self._io.is_eof():
                self.headers.append(Http.Header(self._io, self, self._root))
                i += 1



    class HttpResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.status_code = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.reason_phrase = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")
            self.headers = []
            i = 0
            while not self._io.is_eof():
                self.headers.append(Http.Header(self._io, self, self._root))
                i += 1



    class Header(KaitaiStruct):
        """Each header is a name-value pair. Names are case-insensitive.
        The value is terminated by CRLF."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(14880, False, True, True)).decode(u"ASCII")
            self.value = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")



