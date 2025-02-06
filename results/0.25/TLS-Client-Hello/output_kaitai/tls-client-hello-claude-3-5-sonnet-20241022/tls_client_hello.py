# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

    class NamedGroup(Enum):
        secp256r1 = 23
        secp384r1 = 24
        secp521r1 = 25
        x25519 = 29
        x448 = 30
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.msg_type = self._io.read_u1()
        if not self.msg_type == 1:
            raise kaitaistruct.ValidationNotEqualError(1, self.msg_type, self._io, u"/seq/0")
        self.length = self._io.read_bits_int_be(24)
        self._io.align_to_byte()
        self.legacy_version = self._io.read_u2be()
        if not self.legacy_version == 771:
            raise kaitaistruct.ValidationNotEqualError(771, self.legacy_version, self._io, u"/seq/2")
        self.random = self._io.read_bytes(32)
        self.legacy_session_id_length = self._io.read_u1()
        self.legacy_session_id = self._io.read_bytes(self.legacy_session_id_length)
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


    class ProtocolName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_length = self._io.read_u1()
            self.name = self._io.read_bytes(self.name_length)


    class PreSharedKeyExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identities_length = self._io.read_u2be()
            self.identities = []
            i = 0
            while not self._io.is_eof():
                self.identities.append(TlsClientHello.PskIdentity(self._io, self, self._root))
                i += 1



    class PskModesExtension(KaitaiStruct):

        class PskKeyExchangeMode(Enum):
            psk_ke = 0
            psk_dhe_ke = 1
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ke_modes_length = self._io.read_u1()
            self.ke_modes = []
            for i in range(self.ke_modes_length):
                self.ke_modes.append(KaitaiStream.resolve_enum(TlsClientHello.PskModesExtension.PskKeyExchangeMode, self._io.read_u1()))



    class PskIdentity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identity_length = self._io.read_u2be()
            self.identity = self._io.read_bytes(self.identity_length)
            self.obfuscated_ticket_age = self._io.read_u4be()


    class ServerNameExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.server_name_list_length = self._io.read_u2be()
            self.server_name_list = []
            i = 0
            while not self._io.is_eof():
                self.server_name_list.append(TlsClientHello.ServerNameEntry(self._io, self, self._root))
                i += 1



    class ServerNameEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.name_length = self._io.read_u2be()
            self.name = self._io.read_bytes(self.name_length)


    class SupportedGroupsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.supported_groups_length = self._io.read_u2be()
            self.supported_groups = []
            for i in range(self.supported_groups_length // 2):
                self.supported_groups.append(KaitaiStream.resolve_enum(TlsClientHello.NamedGroup, self._io.read_u2be()))



    class SupportedVersionsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.versions_length = self._io.read_u1()
            self.versions = []
            for i in range(self.versions_length // 2):
                self.versions.append(self._io.read_u2be())



    class KeyShareExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.client_shares_length = self._io.read_u2be()
            self.client_shares = []
            i = 0
            while not self._io.is_eof():
                self.client_shares.append(TlsClientHello.KeyShareEntry(self._io, self, self._root))
                i += 1



    class SignatureAlgorithmsExtension(KaitaiStruct):

        class SignatureScheme(Enum):
            ecdsa_secp256r1_sha256 = 1027
            ecdsa_secp384r1_sha384 = 1283
            ecdsa_secp521r1_sha512 = 1539
            rsa_pss_rsae_sha256 = 2052
            rsa_pss_rsae_sha384 = 2053
            rsa_pss_rsae_sha512 = 2054
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.supported_signature_algorithms_length = self._io.read_u2be()
            self.supported_signature_algorithms = []
            for i in range(self.supported_signature_algorithms_length // 2):
                self.supported_signature_algorithms.append(KaitaiStream.resolve_enum(TlsClientHello.SignatureAlgorithmsExtension.SignatureScheme, self._io.read_u2be()))



    class CookieExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cookie_length = self._io.read_u2be()
            self.cookie = self._io.read_bytes(self.cookie_length)


    class CipherSuite(KaitaiStruct):

        class CipherSuiteEnum(Enum):
            tls_aes_128_gcm_sha256 = 4865
            tls_aes_256_gcm_sha384 = 4866
            tls_chacha20_poly1305_sha256 = 4867
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = KaitaiStream.resolve_enum(TlsClientHello.CipherSuite.CipherSuiteEnum, self._io.read_u2be())


    class KeyShareEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = KaitaiStream.resolve_enum(TlsClientHello.NamedGroup, self._io.read_u2be())
            self.key_exchange_length = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.key_exchange_length)


    class AlpnExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.alpn_list_length = self._io.read_u2be()
            self.alpn_list = []
            i = 0
            while not self._io.is_eof():
                self.alpn_list.append(TlsClientHello.ProtocolName(self._io, self, self._root))
                i += 1



    class EarlyDataExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class Extension(KaitaiStruct):

        class ExtensionType(Enum):
            server_name = 0
            supported_groups = 10
            signature_algorithms = 13
            alpn = 16
            pre_shared_key = 41
            early_data = 42
            supported_versions = 43
            cookie = 44
            psk_key_exchange_modes = 45
            key_share = 51
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(TlsClientHello.Extension.ExtensionType, self._io.read_u2be())
            self.length = self._io.read_u2be()
            _on = self.type
            if _on == TlsClientHello.Extension.ExtensionType.key_share:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.KeyShareExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.supported_versions:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedVersionsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.alpn:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.AlpnExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.psk_key_exchange_modes:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PskModesExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.server_name:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.ServerNameExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.supported_groups:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedGroupsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.signature_algorithms:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SignatureAlgorithmsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.early_data:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.EarlyDataExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.cookie:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.CookieExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.Extension.ExtensionType.pre_shared_key:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PreSharedKeyExtension(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes(self.length)



