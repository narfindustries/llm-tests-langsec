# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

    class CipherSuites(Enum):
        tls_aes_128_gcm_sha256 = 4865
        tls_aes_256_gcm_sha384 = 4866
        tls_chacha20_poly1305_sha256 = 4867

    class LegacyCompressionMethods(Enum):
        no_compression = 0

    class ExtensionsType(Enum):
        server_name = 0
        supported_groups = 10
        application_layer_protocol_negotiation = 16
        pre_shared_key = 35
        supported_versions = 43
        key_share = 51
        psk_key_exchange_modes = 61
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.legacy_version = self._io.read_u2be()
        self.random = self._io.read_bytes(32)
        self.legacy_session_id = TlsClientHello.SessionId(self._io, self, self._root)
        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = self._io.read_bytes(self.cipher_suites_length)
        self.legacy_compression_methods_length = self._io.read_u1()
        self.legacy_compression_methods = self._io.read_bytes(self.legacy_compression_methods_length)
        self.extensions_length = self._io.read_u2be()
        self._raw_extensions = self._io.read_bytes(self.extensions_length)
        _io__raw_extensions = KaitaiStream(BytesIO(self._raw_extensions))
        self.extensions = TlsClientHello.Extensions(_io__raw_extensions, self, self._root)

    class SessionId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.session_id = self._io.read_bytes(self.length)


    class Extensions(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_list = []
            i = 0
            while not self._io.is_eof():
                self.extension_list.append(TlsClientHello.Extension(self._io, self, self._root))
                i += 1



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



