# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http(KaitaiStruct):
    """HTTP/1.1 is a version of the Hypertext Transfer Protocol used by the World Wide Web.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.request = Http.RequestLine(self._io, self, self._root)
        self.response = Http.ResponseLine(self._io, self, self._root)
        self.headers = []
        i = 0
        while not self._io.is_eof():
            self.headers.append(Http.Headers(self._io, self, self._root))
            i += 1


    class RequestLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.uri = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.version = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class ResponseLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.version = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.status_code = self._io.read_u1()
            self.reason_phrase = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class Headers(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.value = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class Strz(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.str = (self._io.read_bytes_term(3338, False, True, True)).decode(u"ASCII")



