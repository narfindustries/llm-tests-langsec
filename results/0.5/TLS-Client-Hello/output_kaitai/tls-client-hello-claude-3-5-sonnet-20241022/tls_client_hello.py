# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):

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
        self.protocol_version = self._io.read_u2be()
        if not self.protocol_version == 771:
            raise kaitaistruct.ValidationNotEqualError(771, self.protocol_version, self._io, u"/seq/0")
        self.random = self._io.read_bytes(32)
        self.legacy_session_id = TlsClientHello.LegacySessionIdT(self._io, self, self._root)
        self.cipher_suites = TlsClientHello.CipherSuitesT(self._io, self, self._root)
        self.legacy_compression_methods = TlsClientHello.LegacyCompressionMethodsT(self._io, self, self._root)
        self.extensions = TlsClientHello.ExtensionsT(self._io, self, self._root)

    class PreSharedKeyExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identities_len = self._io.read_u2be()
            self.identities = []
            i = 0
            while not self._io.is_eof():
                self.identities.append(TlsClientHello.PskIdentity(self._io, self, self._root))
                i += 1

            self.binders_len = self._io.read_u2be()
            self.binders = []
            i = 0
            while not self._io.is_eof():
                self.binders.append(TlsClientHello.PskBinder(self._io, self, self._root))
                i += 1



    class PskBinder(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            self.binder = self._io.read_bytes(self.len)


    class PskIdentity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identity_len = self._io.read_u2be()
            self.identity = self._io.read_bytes(self.identity_len)
            self.obfuscated_ticket_age = self._io.read_u4be()


    class ServerNameListT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.name_len = self._io.read_u2be()
            self.name = self._io.read_bytes(self.name_len)


    class CipherSuitesT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.cipher_suites = []
            for i in range(self.len // 2):
                self.cipher_suites.append(self._io.read_u2be())



    class PskKeyExchangeModesExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            self.modes = self._io.read_bytes(self.len)


    class ServerNameExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self._raw_server_name_list = self._io.read_bytes(self.len)
            _io__raw_server_name_list = KaitaiStream(BytesIO(self._raw_server_name_list))
            self.server_name_list = TlsClientHello.ServerNameListT(_io__raw_server_name_list, self, self._root)


    class ExtensionsT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.extensions = []
            i = 0
            while not self._io.is_eof():
                self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))
                i += 1



    class LegacySessionIdT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            self.session_id = self._io.read_bytes(self.len)


    class SupportedGroupsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.groups = []
            for i in range(self.len // 2):
                self.groups.append(self._io.read_u2be())



    class SupportedVersionsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            self.versions = []
            for i in range(self.len // 2):
                self.versions.append(self._io.read_u2be())



    class KeyShareExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.client_shares = []
            i = 0
            while not self._io.is_eof():
                self.client_shares.append(TlsClientHello.KeyShareEntry(self._io, self, self._root))
                i += 1



    class ProtocolNameListT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.protocol_name_len = self._io.read_u1()
            self.protocol_name = self._io.read_bytes(self.protocol_name_len)


    class SignatureAlgorithmsExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.algorithms = []
            for i in range(self.len // 2):
                self.algorithms.append(self._io.read_u2be())



    class CookieExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cookie_len = self._io.read_u2be()
            self.cookie = self._io.read_bytes(self.cookie_len)


    class KeyShareEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2be()
            self.key_exchange_len = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.key_exchange_len)


    class AlpnExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self._raw_protocol_name_list = self._io.read_bytes(self.len)
            _io__raw_protocol_name_list = KaitaiStream(BytesIO(self._raw_protocol_name_list))
            self.protocol_name_list = TlsClientHello.ProtocolNameListT(_io__raw_protocol_name_list, self, self._root)


    class EarlyDataExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class LegacyCompressionMethodsT(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            self.compression_methods = self._io.read_bytes(self.len)


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(TlsClientHello.ExtensionType, self._io.read_u2be())
            self.len = self._io.read_u2be()
            _on = self.type
            if _on == TlsClientHello.ExtensionType.key_share:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.KeyShareExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.supported_versions:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedVersionsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.alpn:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.AlpnExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.psk_key_exchange_modes:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PskKeyExchangeModesExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.server_name:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.ServerNameExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.supported_groups:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SupportedGroupsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.signature_algorithms:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.SignatureAlgorithmsExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.early_data:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.EarlyDataExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.cookie:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.CookieExtension(_io__raw_body, self, self._root)
            elif _on == TlsClientHello.ExtensionType.pre_shared_key:
                self._raw_body = self._io.read_bytes(self.len)
                _io__raw_body = KaitaiStream(BytesIO(self._raw_body))
                self.body = TlsClientHello.PreSharedKeyExtension(_io__raw_body, self, self._root)
            else:
                self.body = self._io.read_bytes(self.len)



