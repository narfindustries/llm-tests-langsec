# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

    class CipherSuitesEnum(Enum):
        tls_aes_128_gcm_sha256 = 4865
        tls_aes_256_gcm_sha384 = 4866
        tls_chacha20_poly1305_sha256 = 4867
        tls_aes_128_ccm_sha256 = 4868
        tls_aes_128_ccm_8_sha256 = 4869

    class ExtensionTypeEnum(Enum):
        server_name = 0
        supported_groups = 10
        signature_algorithms = 13
        pre_shared_key = 41
        supported_versions = 43
        psk_key_exchange_modes = 45
        key_share = 51

    class SupportedGroupsEnum(Enum):
        secp256r1 = 23
        secp384r1 = 24
        secp521r1 = 25
        x25519 = 29
        x448 = 30

    class SignatureAlgorithmsEnum(Enum):
        ecdsa_secp256r1_sha256 = 1027
        ecdsa_secp384r1_sha384 = 1283
        ecdsa_secp521r1_sha512 = 1539
        ed25519 = 2055
        ed448 = 2056
        rsa_pss_pss_sha256 = 2057
        rsa_pss_pss_sha384 = 2058
        rsa_pss_pss_sha512 = 2059
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.handshake_type = self._io.read_bytes(1)
        if not self.handshake_type == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.handshake_type, self._io, u"/seq/0")
        self.length = self._io.read_bits_int_be(24)
        self._io.align_to_byte()
        self._raw_msg_content = self._io.read_bytes(self.length)
        _io__raw_msg_content = KaitaiStream(BytesIO(self._raw_msg_content))
        self.msg_content = TlsClientHello.ClientHello(_io__raw_msg_content, self, self._root)

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
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.psk_modes_length = self._io.read_u1()
            self.psk_modes = []
            for i in range(self.psk_modes_length):
                self.psk_modes.append(self._io.read_u1())



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
                self.supported_groups.append(KaitaiStream.resolve_enum(TlsClientHello.SupportedGroupsEnum, self._io.read_u2be()))



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



    class ClientHello(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.legacy_version = self._io.read_bytes(2)
            if not self.legacy_version == b"\x03\x03":
                raise kaitaistruct.ValidationNotEqualError(b"\x03\x03", self.legacy_version, self._io, u"/types/client_hello/seq/0")
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



    class SignatureAlgorithmsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.supported_signature_algorithms_length = self._io.read_u2be()
            self.supported_signature_algorithms = []
            for i in range(self.supported_signature_algorithms_length // 2):
                self.supported_signature_algorithms.append(KaitaiStream.resolve_enum(TlsClientHello.SignatureAlgorithmsEnum, self._io.read_u2be()))



    class CipherSuite(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cipher_suite = KaitaiStream.resolve_enum(TlsClientHello.CipherSuitesEnum, self._io.read_u2be())


    class KeyShareEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = KaitaiStream.resolve_enum(TlsClientHello.SupportedGroupsEnum, self._io.read_u2be())
            self.key_exchange_length = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.key_exchange_length)


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = KaitaiStream.resolve_enum(TlsClientHello.ExtensionTypeEnum, self._io.read_u2be())
            self.extension_length = self._io.read_u2be()
            _on = self.extension_type
            if _on == TlsClientHello.ExtensionTypeEnum.supported_groups:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.SupportedGroupsExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.server_name:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.ServerNameExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.pre_shared_key:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.PreSharedKeyExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.psk_key_exchange_modes:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.PskModesExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.signature_algorithms:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.SignatureAlgorithmsExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.supported_versions:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.SupportedVersionsExtension(_io__raw_extension_data, self, self._root)
            elif _on == TlsClientHello.ExtensionTypeEnum.key_share:
                self._raw_extension_data = self._io.read_bytes(self.extension_length)
                _io__raw_extension_data = KaitaiStream(BytesIO(self._raw_extension_data))
                self.extension_data = TlsClientHello.KeyShareExtension(_io__raw_extension_data, self, self._root)
            else:
                self.extension_data = self._io.read_bytes(self.extension_length)



