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
        self.client_version = self._io.read_u2be()
        self.random = self._io.read_bytes(32)
        self.session_id_length = self._io.read_u1()
        self.session_id = self._io.read_bytes(self.session_id_length)
        self.cipher_suites_length = self._io.read_u2be()
        self.cipher_suites = []
        for i in range(self.cipher_suites_length // 2):
            self.cipher_suites.append(TlsClientHello.CipherSuite(self._io, self, self._root))

        self.compression_methods_length = self._io.read_u1()
        self.compression_methods = []
        for i in range(self.compression_methods_length):
            self.compression_methods.append(self._io.read_u1())

        self.extensions_length = self._io.read_u2be()
        self.extensions = []
        for i in range(self.extensions_length):
            self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))


    class CipherSuite(KaitaiStruct):
        """Supported cipher suites."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cipher = self._io.read_u2be()


    class Extension(KaitaiStruct):
        """Supported extensions."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = self._io.read_u2be()
            self.extension_length = self._io.read_u2be()
            self.extension_data = self._io.read_bytes(self.extension_length)



