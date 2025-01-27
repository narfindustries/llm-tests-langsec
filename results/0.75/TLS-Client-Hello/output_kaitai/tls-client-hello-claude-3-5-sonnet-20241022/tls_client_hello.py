# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

    class ContentType(Enum):
        change_cipher_spec = 20
        alert = 21
        handshake = 22
        application_data = 23

    class HandshakeType(Enum):
        client_hello = 1
        server_hello = 2
        certificate = 11
        server_key_exchange = 12
        server_hello_done = 14
        client_key_exchange = 16

    class ExtensionType(Enum):
        server_name = 0
        supported_groups = 10
        ec_point_formats = 11
        signature_algorithms = 13
        application_layer_protocol_negotiation = 16
        supported_versions = 43
        psk_key_exchange_modes = 45
        key_share = 51
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.record_header = TlsClientHello.RecordHeader(self._io, self, self._root)
        self.handshake_header = TlsClientHello.HandshakeHeader(self._io, self, self._root)
        self.client_version = TlsClientHello.Version(self._io, self, self._root)
        self.random = TlsClientHello.Random(self._io, self, self._root)
        self.session_id_length = self._io.read_u1()
        if self.session_id_length > 0:
            self.session_id = self._io.read_bytes(self.session_id_length)

        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = []
        for i in range(self.cipher_suites_length // 2):
            self.cipher_suites.append(TlsClientHello.CipherSuite(self._io, self, self._root))

        self.compression_methods_length = self._io.read_u1()
        self.compression_methods = self._io.read_bytes(self.compression_methods_length)
        self.extensions_length = self._io.read_u2be()
        self.extensions = []
        i = 0
        while not self._io.is_eof():
            self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))
            i += 1


    class Random(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gmt_unix_time = self._io.read_u4be()
            self.random_bytes = self._io.read_bytes(28)


    class Version(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.major = self._io.read_u1()
            self.minor = self._io.read_u1()


    class HandshakeHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.msg_type = self._io.read_u1()
            self.length = self._io.read_bits_int_be(24)


    class RecordHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content_type = self._io.read_u1()
            self.version = TlsClientHello.Version(self._io, self, self._root)
            self.length = self._io.read_u2be()


    class CipherSuite(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cipher_suite = self._io.read_u2be()


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u2be()
            self.length = self._io.read_u2be()
            self.data = self._io.read_bytes(self.length)



