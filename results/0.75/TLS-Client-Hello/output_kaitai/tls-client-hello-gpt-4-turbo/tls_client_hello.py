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
        self.session_id_length = self._io.read_u1()
        self.session_id = self._io.read_bytes(self.session_id_length)
        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = []
        for i in range(self.cipher_suites_length // 2):
            self.cipher_suites.append(TlsClientHello.CipherSuite(self._io, self, self._root))

        self.compression_methods_length = self._io.read_u1()
        self.compression_methods = self._io.read_bytes(self.compression_methods_length)
        self.extensions_length = self._io.read_u2be()
        self._raw_extensions = []
        self.extensions = []
        i = 0
        while True:
            _buf = self._io.read_bytes_full()
            self._raw_extensions.append(_buf)
            _io__raw_extensions = KaitaiStream(BytesIO(self._raw_extensions[-1]))
            _ = TlsClientHello.TlsExtension(_io__raw_extensions, self, self._root)
            self.extensions.append(_)
            if _.length == 0:
                break
            i += 1

    class ServerName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.server_name_list = []
            for i in range(self.list_length // 5):
                self.server_name_list.append(TlsClientHello.ServerNameType(self._io, self, self._root))



    class SignatureAlgorithms(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.algorithms = []
            for i in range(self.list_length // 2):
                self.algorithms.append(self._io.read_u2be())



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
                self.data = TlsClientHello.SupportedGroups(_io__raw_data, self, self._root)
            elif _on == 0:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.ServerName(_io__raw_data, self, self._root)
            elif _on == 24:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SupportedVersions(_io__raw_data, self, self._root)
            elif _on == 1:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 27:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 13:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.SignatureAlgorithms(_io__raw_data, self, self._root)
            elif _on == 11:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 5:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 23:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 21:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 28:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 16:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 18:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 26:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 31:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.KeyShare(_io__raw_data, self, self._root)
            elif _on == 2:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 29:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 25:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            elif _on == 22:
                self._raw_data = self._io.read_bytes(self.length)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = TlsClientHello.GenericExtension(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes(self.length)


    class KeyExchangeEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2be()
            self.key_exchange_length = self._io.read_u2be()
            self.key_exchange = self._io.read_bytes(self.key_exchange_length)


    class KeyShare(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.list_length = self._io.read_u2be()
            self.key_exchanges = []
            for i in range(self.list_length // 4):
                self.key_exchanges.append(TlsClientHello.KeyExchangeEntry(self._io, self, self._root))



    class GenericExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.contents = []
            for i in range(self._parent.length):
                self.contents.append(self._io.read_u1())



    class SupportedGroups(KaitaiStruct):
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



    class CipherSuite(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()


    class SupportedVersions(KaitaiStruct):
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



    class ServerNameType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_type = self._io.read_u1()
            self.name_length = self._io.read_u2be()
            self.server_name = (self._io.read_bytes(self.name_length)).decode(u"UTF-8")



