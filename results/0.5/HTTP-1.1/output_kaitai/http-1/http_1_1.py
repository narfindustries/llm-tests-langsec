# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Http11(KaitaiStruct):

    class HttpMethods(Enum):
        get = 1
        post = 2
        put = 3
        delete = 4
        head = 5
        options = 6
        trace = 7
        connect = 8
        patch = 9

    class HeaderNames(Enum):
        accept = 1
        accept_charset = 2
        accept_encoding = 3
        accept_language = 4
        authorization = 5
        host = 6
        user_agent = 7
        connection = 8
        content_type = 9
        content_length = 10
        server = 11

    class StatusCodes(Enum):
        ok = 200
        created = 201
        no_content = 204
        moved_permanently = 301
        bad_request = 400
        unauthorized = 401
        forbidden = 403
        not_found = 404
        internal_server_error = 500
        bad_gateway = 502
        service_unavailable = 503
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.request_line = Http11.RequestLine(self._io, self, self._root)
        self.headers = []
        i = 0
        while not self._io.is_eof():
            self.headers.append(Http11.Header(self._io, self, self._root))
            i += 1


    class Body(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"utf-8")


    class RequestLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"utf-8")
            self.uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"utf-8")
            self.http_version = (self._io.read_bytes_term(3338, False, True, True)).decode(u"utf-8")


    class Response(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.status_line = Http11.StatusLine(self._io, self, self._root)
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
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"utf-8")
            self.value = (self._io.read_bytes_term(3338, False, True, True)).decode(u"utf-8")


    class StatusLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.http_version = (self._io.read_bytes_term(32, False, True, True)).decode(u"utf-8")
            self.status_code = KaitaiStream.resolve_enum(Http11.StatusCodes, self._io.read_u2be())
            self.reason_phrase = (self._io.read_bytes_term(3338, False, True, True)).decode(u"utf-8")



