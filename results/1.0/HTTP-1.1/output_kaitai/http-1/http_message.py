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
        self.headers = []
        i = 0
        while True:
            _ = HttpMessage.Header(self._io, self, self._root)
            self.headers.append(_)
            if _.name == u"":
                break
            i += 1
        if not (self._io.is_eof()):
            self.body = HttpMessage.Body(self._io, self, self._root)


    class StartLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method_or_version = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.request_target_or_status = (self._io.read_bytes_term(32, False, True, True)).decode(u"ASCII")
            self.version_or_reason = (self._io.read_bytes_term(10, False, True, True)).decode(u"ASCII")


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"ASCII")
            if self.name != u"":
                self.space = self._io.read_u1()

            if self.name != u"":
                self.value = (self._io.read_bytes_term(10, False, True, True)).decode(u"ASCII")



    class Body(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = self._io.read_bytes_full()



