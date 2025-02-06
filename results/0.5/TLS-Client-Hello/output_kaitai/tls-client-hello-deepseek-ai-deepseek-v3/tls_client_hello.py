# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class TlsClientHello(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.legacy_version = self._io.read_u2be()
        self.random = self._io.read_bytes(32)
        self.legacy_session_id = TlsClientHello.SessionId(self._io, self, self._root)
        self.cipher_suites = TlsClientHello.CipherSuites(self._io, self, self._root)
        self.legacy_compression_methods = TlsClientHello.CompressionMethods(self._io, self, self._root)
        self.extensions = TlsClientHello.Extensions(self._io, self, self._root)

    class ProtocolName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.name = self._io.read_bytes(self.length)


    class ServerName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.server_name_list = []
            i = 0
            while not self._io.is_eof():
                self.server_name_list.append(TlsClientHello.ServerNameEntry(self._io, self, self._root))
                i += 1



    class PskBinder(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.binder_length = self._io.read_u1()
            self.binder = self._io.read_bytes(self.binder_length)


    class SignatureAlgorithms(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.signature_algorithms = []
            for i in range(self.list_length // 2):
                self.signature_algorithms.append(self._io.read_u2be())



    class SessionId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.session_id = self._io.read_bytes(self.length)


    class CipherSuites(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.cipher_suites = []
            for i in range(self.length // 2):
                self.cipher_suites.append(self._io.read_u2be())



    class PskKeyExchangeModes(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u1()
            self.modes = []
            for i in range(self.list_length):
                self.modes.append(self._io.read_u1())



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


    class CompressionMethods(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.compression_methods = []
            for i in range(self.length):
                self.compression_methods.append(self._io.read_u1())



    class ServerNameEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.length = self._io.read_u2be()
            self.host_name = self._io.read_bytes(self.length)


    class KeyShare(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.client_shares_length = self._io.read_u2be()
            self.client_shares = []
            for i in range(self.client_shares_length):
                self.client_shares.append(TlsClientHello.KeyShareEntry(self._io, self, self._root))



    class Alpn(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.protocol_name_list = []
            i = 0
            while not self._io.is_eof():
                self.protocol_name_list.append(TlsClientHello.ProtocolName(self._io, self, self._root))
                i += 1



    class Extensions(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.extensions = []
            i = 0
            while not self._io.is_eof():
                self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))
                i += 1



    class PreSharedKey(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identities_length = self._io.read_u2be()
            self.identities = []
            for i in range(self.identities_length):
                self.identities.append(TlsClientHello.PskIdentity(self._io, self, self._root))

            self.binders_length = self._io.read_u2be()
            self.binders = []
            for i in range(self.binders_length):
                self.binders.append(TlsClientHello.PskBinder(self._io, self, self._root))



    class SupportedGroups(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.supported_groups = []
            for i in range(self.list_length // 2):
                self.supported_groups.append(self._io.read_u2be())



    class KeyShareEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2be()
            self.key_exchange_length = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.key_exchange_length)


    class SupportedVersions(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u1()
            self.supported_versions = []
            for i in range(self.list_length // 2):
                self.supported_versions.append(self._io.read_u2be())



    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u2be()
            self.length = self._io.read_u2be()
            _on = self.type
            if _on == 10:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SupportedGroups(_io__raw_data, self, self._root)
            elif _on == 0:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.ServerName(_io__raw_data, self, self._root)
            elif _on == 13:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SignatureAlgorithms(_io__raw_data, self, self._root)
            elif _on == 45:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.PskKeyExchangeModes(_io__raw_data, self, self._root)
            elif _on == 51:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.KeyShare(_io__raw_data, self, self._root)
            elif _on == 41:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.PreSharedKey(_io__raw_data, self, self._root)
            elif _on == 16:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.Alpn(_io__raw_data, self, self._root)
            elif _on == 43:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SupportedVersions(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)



