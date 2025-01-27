# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http11(KaitaiStruct):
    """A basic parser for the HTTP 1.1 protocol, focusing on the structure of HTTP requests and responses.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.request = Http11.HttpRequest(self._io, self, self._root)
        self.response = Http11.HttpResponse(self._io, self, self._root)

    class HttpRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.version = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")
            self.newline = self._io.read_bytes(1)
            self.headers = []
            i = 0
            while True:
                _ = Http11.Header(self._io, self, self._root)
                self.headers.append(_)
                if _.name == u"":
                    break
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
            self.reason_phrase = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")
            self.newline = self._io.read_bytes(1)
            self.headers = []
            i = 0
            while True:
                _ = Http11.Header(self._io, self, self._root)
                self.headers.append(_)
                if _.name == u"":
                    break
                i += 1


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"ASCII")
            self.space = self._io.read_bytes(1)
            self.value = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")
            self.newline = self._io.read_bytes(1)



