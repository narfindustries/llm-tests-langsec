# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

    class ExtensionType(Enum):
        server_name_type = 0
        supported_groups = 10
        ec_point_formats = 11
        signature_algorithms = 13
        alpn = 16
        signed_certificate_timestamp = 18
        extended_master_secret = 23
        key_share = 33
        pre_shared_key = 41
        early_data = 42
        supported_versions = 43
        cookie = 44
        psk_key_exchange_modes = 45
        renegotiation_info = 65281
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.legacy_version = self._io.read_u2be()
        self.random = self._io.read_bytes(32)
        self.legacy_session_id_length = self._io.read_u1()
        self.legacy_session_id = self._io.read_bytes(self.legacy_session_id_length)
        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = []
        for i in range(self.cipher_suites_length // 2):
            self.cipher_suites.append(self._io.read_u2be())

        self.legacy_compression_methods_length = self._io.read_u1()
        self.legacy_compression_methods = self._io.read_bytes(self.legacy_compression_methods_length)
        self.extensions_length = self._io.read_u2be()
        self.extensions = []
        for i in range(len(self.extensions)):
            self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))


    class ServerName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.len_hostname = self._io.read_u2be()
            self.hostname = self._io.read_bytes(self.len_hostname)


    class EarlyDataExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class PskIdentity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_identity = self._io.read_u2be()
            self.identity = self._io.read_bytes(self.len_identity)
            self.obfuscated_ticket_age = self._io.read_u4be()


    class SignatureAlgorithmsExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.algorithms_length = self._io.read_u2be()
            self.algorithms = []
            for i in range(self.algorithms_length // 2):
                self.algorithms.append(self._io.read_u2be())



    class OpaqueVector(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_data = self._io.read_u2be()
            self.data = self._io.read_bytes(self.len_data)


    class KeyShareExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_client_shares = self._io.read_u2be()
            self.client_shares = []
            for i in range(self.num_client_shares):
                self.client_shares.append(TlsClientHello.KeyShareEntry(self._io, self, self._root))



    class RawExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.raw_data = self._io.read_bytes_full()


    class PskKeyExchangeModesExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_modes = self._io.read_u1()
            self.modes = []
            for i in range(self.num_modes):
                self.modes.append(self._io.read_u1())



    class PreSharedKeyExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_identities = self._io.read_u2be()
            self.identities = []
            for i in range(self.num_identities):
                self.identities.append(TlsClientHello.PskIdentity(self._io, self, self._root))

            self.num_binders = self._io.read_u2be()
            self.binders = []
            for i in range(self.num_binders):
                self.binders.append(TlsClientHello.OpaqueVector(self._io, self, self._root))



    class KeyShareEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2be()
            self.len_key_exchange = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.len_key_exchange)


    class SupportedGroupsExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups_length = self._io.read_u2be()
            self.groups = []
            for i in range(self.groups_length // 2):
                self.groups.append(self._io.read_u2be())



    class ServerNameExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_server_names = self._io.read_u2be()
            self.server_names = []
            for i in range(self.num_server_names):
                self.server_names.append(TlsClientHello.ServerName(self._io, self, self._root))



    class SupportedVersionsExt(KaitaiStruct):
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



    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(TlsClientHello.ExtensionType, self._io.read_u2be())
            self.length = self._io.read_u2be()
            _on = self.type
            if _on == TlsClientHello.ExtensionType.server_name_type:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.ServerNameExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.key_share:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.KeyShareExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.supported_versions:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedVersionsExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.psk_key_exchange_modes:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PskKeyExchangeModesExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.supported_groups:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedGroupsExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.signature_algorithms:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SignatureAlgorithmsExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.early_data:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.EarlyDataExt(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.pre_shared_key:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PreSharedKeyExt(_io__raw_body, self, self._root)
            else:
                self._raw_body = self._io.read_bytes(self.length)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.RawExtension(_io__raw_body, self, self._root)



