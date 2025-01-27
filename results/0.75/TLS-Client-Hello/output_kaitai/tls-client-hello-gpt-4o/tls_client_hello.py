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
        self.record_length = self._io.read_u2be()
        self.message_type = self._io.read_u1()
        self.version = TlsClientHello.Version(self._io, self, self._root)
        self.random = TlsClientHello.Random(self._io, self, self._root)
        self.session_id_length = self._io.read_u1()
        self.session_id = self._io.read_bytes(self.session_id_length)
        self.cipher_suites_length = self._io.read_u2be()
        self._raw_cipher_suites = self._io.read_bytes(self.cipher_suites_length)
        _io__raw_cipher_suites = KaitaiStream(BytesIO(self._raw_cipher_suites))
        self.cipher_suites = TlsClientHello.CipherSuites(_io__raw_cipher_suites, self, self._root)
        self.compression_methods_length = self._io.read_u1()
        self.compression_methods = self._io.read_bytes(self.compression_methods_length)
        self.extensions_length = self._io.read_u2be()
        self.extensions = self._io.read_bytes(self.extensions_length)

    class Version(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.major = self._io.read_u1()
            self.minor = self._io.read_u1()


    class Random(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gmt_unix_time = self._io.read_u4be()
            self.random_bytes = self._io.read_bytes(28)


    class CipherSuites(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.suites = []
            i = 0
            while not self._io.is_eof():
                self.suites.append(self._io.read_u2be())
                i += 1




