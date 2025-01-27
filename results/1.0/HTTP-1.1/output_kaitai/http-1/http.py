# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.request_line = Http.RequestLine(self._io, self, self._root)

    class RequestLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes(4)).decode(u"ascii")
            self.space1 = (self._io.read_bytes(1)).decode(u"ascii")
            self.url = (self._io.read_bytes(100)).decode(u"ascii")
            self.space2 = (self._io.read_bytes(1)).decode(u"ascii")
            self.version = (self._io.read_bytes(8)).decode(u"ascii")
            self.crlf = (self._io.read_bytes(2)).decode(u"ascii")


    class Headers(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes(20)).decode(u"ascii")
            self.colon = (self._io.read_bytes(1)).decode(u"ascii")
            self.space = (self._io.read_bytes(1)).decode(u"ascii")
            self.value = (self._io.read_bytes(100)).decode(u"ascii")
            self.crlf = (self._io.read_bytes(2)).decode(u"ascii")


    class Body(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes(1000)).decode(u"ascii")



