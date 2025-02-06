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
        head = 3
        put = 4
        delete = 5
        trace = 6
        options = 7
        connect = 8
        patch = 9

    class HttpVersions(Enum):
        http_1_0 = 1
        http_1_1 = 2

    class HttpStatusCodes(Enum):
        continue = 100
        switching_protocols = 101
        ok = 200
        created = 201
        accepted = 202
        non_authoritative_information = 203
        no_content = 204
        reset_content = 205
        partial_content = 206
        multiple_choices = 300
        moved_permanently = 301
        found = 302
        see_other = 303
        not_modified = 304
        use_proxy = 305
        temporary_redirect = 307
        bad_request = 400
        unauthorized = 401
        payment_required = 402
        forbidden = 403
        not_found = 404
        method_not_allowed = 405
        not_acceptable = 406
        proxy_authentication_required = 407
        request_timeout = 408
        conflict = 409
        gone = 410
        length_required = 411
        precondition_failed = 412
        request_entity_too_large = 413
        request_uri_too_long = 414
        unsupported_media_type = 415
        requested_range_not_satisfiable = 416
        expectation_failed = 417
        internal_server_error = 500
        not_implemented = 501
        bad_gateway = 502
        service_unavailable = 503
        gateway_timeout = 504
        http_version_not_supported = 505
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.message = Http11.Message(self._io, self, self._root)

    class GeneralHeaders(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if True:
                self.cache_control = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.connection = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.date = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.pragma = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.trailer = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.transfer_encoding = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.upgrade = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.via = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.warning = Http11.HeaderField(self._io, self, self._root)



    class Body(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"ascii")


    class ResponseHeaders(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if True:
                self.accept_ranges = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.age = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.etag = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.location = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.proxy_authenticate = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.retry_after = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.server = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.vary = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.www_authenticate = Http11.HeaderField(self._io, self, self._root)



    class EntityHeaders(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if True:
                self.allow = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_encoding = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_language = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_length = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_location = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_md5 = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_range = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.content_type = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.expires = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.last_modified = Http11.HeaderField(self._io, self, self._root)



    class RequestHeaders(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if True:
                self.accept = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.accept_charset = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.accept_encoding = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.accept_language = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.authorization = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.expect = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.from = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.host = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.if_match = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.if_modified_since = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.if_none_match = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.if_range = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.if_unmodified_since = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.max_forwards = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.proxy_authorization = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.range = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.referer = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.te = Http11.HeaderField(self._io, self, self._root)

            if True:
                self.user_agent = Http11.HeaderField(self._io, self, self._root)



    class HeaderField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = (self._io.read_bytes_term(58, False, True, True)).decode(u"ascii")
            self.value = (self._io.read_bytes_term(13, False, True, True)).decode(u"ascii")


    class RequestMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.request_line = Http11.RequestLine(self._io, self, self._root)
            self.headers = Http11.Headers(self._io, self, self._root)
            self._raw_body = self._io.read_bytes_full()
            _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
            self.body = Http11.Body(_io__raw_body, self, self._root)


    class RequestLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.method = (self._io.read_bytes_term(32, False, True, True)).decode(u"ascii")
            self.request_uri = (self._io.read_bytes_term(32, False, True, True)).decode(u"ascii")
            self.http_version = (self._io.read_bytes_term(13, False, True, True)).decode(u"ascii")


    class Message(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.message_type = (self._io.read_bytes(1)).decode(u"ascii")
            _on = self.message_type
            if _on == u"R":
                self.content = Http11.RequestMessage(self._io, self, self._root)
            elif _on == u"P":
                self.content = Http11.ResponseMessage(self._io, self, self._root)


    class Headers(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.general_headers = Http11.GeneralHeaders(self._io, self, self._root)
            self.request_headers = Http11.RequestHeaders(self._io, self, self._root)
            self.response_headers = Http11.ResponseHeaders(self._io, self, self._root)
            self.entity_headers = Http11.EntityHeaders(self._io, self, self._root)


    class StatusLine(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.http_version = (self._io.read_bytes_term(32, False, True, True)).decode(u"ascii")
            self.status_code = (self._io.read_bytes_term(32, False, True, True)).decode(u"ascii")
            self.reason_phrase = (self._io.read_bytes_term(13, False, True, True)).decode(u"ascii")


    class ResponseMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.status_line = Http11.StatusLine(self._io, self, self._root)
            self.headers = Http11.Headers(self._io, self, self._root)
            self._raw_body = self._io.read_bytes_full()
            _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
            self.body = Http11.Body(_io__raw_body, self, self._root)



