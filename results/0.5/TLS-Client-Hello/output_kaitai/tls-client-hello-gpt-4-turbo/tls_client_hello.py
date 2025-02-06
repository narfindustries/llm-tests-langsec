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
        self.len_legacy_session_id = self._io.read_u1()
        self.legacy_session_id = self._io.read_bytes(self.len_legacy_session_id)
        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = []
        for i in range(self.cipher_suites_length // 2):
            self.cipher_suites.append(TlsClientHello.CipherSuite(self._io, self, self._root))

        self.len_legacy_compression_methods = self._io.read_u1()
        self.legacy_compression_methods = self._io.read_bytes(self.len_legacy_compression_methods)
        self.num_extensions = self._io.read_u2be()
        self.extensions = []
        for i in range(self.num_extensions):
            self.extensions.append(TlsClientHello.TlsExtension(self._io, self, self._root))


    class ServerName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.name_length = self._io.read_u2be()
            self.name = (self._io.read_bytes(self.name_length)).decode(u"UTF-8")


    class KeyShareList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.key_shares = []
            for i in range(self.list_length):
                self.key_shares.append(TlsClientHello.KeyShare(self._io, self, self._root))



    class PskKeyExchangeModes(KaitaiStruct):
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



    class TlsExtension(KaitaiStruct):
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
                self.data = TlsClientHello.SupportedGroupsList(_io__raw_data, self, self._root)
            elif _on == 0:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.ServerNameList(_io__raw_data, self, self._root)
            elif _on == 42:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.EarlyData(_io__raw_data, self, self._root)
            elif _on == 13:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SignatureAlgorithmsList(_io__raw_data, self, self._root)
            elif _on == 45:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.PskKeyExchangeModes(_io__raw_data, self, self._root)
            elif _on == 51:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.KeyShareList(_io__raw_data, self, self._root)
            elif _on == 43:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SupportedVersionsList(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)


    class SupportedVersionsList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u1()
            self.versions = []
            for i in range(self.list_length // 2):
                self.versions.append(self._io.read_u2be())



    class KeyShare(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2be()
            self.len_key_exchange = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.len_key_exchange)


    class CipherSuite(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.suite = self._io.read_u2be()


    class ServerNameList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.server_names = []
            for i in range(self.list_length):
                self.server_names.append(TlsClientHello.ServerName(self._io, self, self._root))



    class EarlyData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.length == 4:
                self.max_early_data_size = self._io.read_u4be()



    class SupportedGroupsList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.groups = []
            for i in range(self.list_length // 2):
                self.groups.append(self._io.read_u2be())



    class SignatureAlgorithmsList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.signatures = []
            for i in range(self.list_length // 2):
                self.signatures.append(self._io.read_u2be())




