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
            while True:
                _ = TlsClientHello.Extension(self._io, self, self._root)
                self.extensions.append(_)
                if _.is_end:
                    break
                i += 1


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = self._io.read_u2be()
            self.extension_data_length = self._io.read_u2be()
            self.extension_data = self._io.read_bytes(self.extension_data_length)

        @property
        def is_end(self):
            if hasattr(self, '_m_is_end'):
                return self._m_is_end

            self._m_is_end = self._io.pos() >= self._io.size()
            return getattr(self, '_m_is_end', None)



