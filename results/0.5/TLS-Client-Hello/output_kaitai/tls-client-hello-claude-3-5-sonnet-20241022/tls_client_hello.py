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
        self.record_header = TlsClientHello.RecordHeader(self._io, self, self._root)
        self.handshake_header = TlsClientHello.HandshakeHeader(self._io, self, self._root)
        self.client_version = TlsClientHello.Version(self._io, self, self._root)
        self.random = TlsClientHello.RandomStruct(self._io, self, self._root)
        self.session_id = TlsClientHello.SessionIdStruct(self._io, self, self._root)
        self.cipher_suites = TlsClientHello.CipherSuitesStruct(self._io, self, self._root)
        self.compression_methods = TlsClientHello.CompressionMethodsStruct(self._io, self, self._root)
        if self._io.pos() < self._io.size():
            self.extensions_length = self._io.read_u2be()

        if self.extensions_length > 0:
            self.extensions = []
            i = 0
            while not self._io.is_eof():
                self.extensions.append(TlsClientHello.Extension(self._io, self, self._root))
                i += 1



    class RandomStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gmt_unix_time = self._io.read_u4be()
            self.random_bytes = self._io.read_bytes(28)


    class CompressionMethodsStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.compression_methods = self._io.read_bytes(self.length)


    class SessionIdStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            if self.length > 0:
                self.session_id = self._io.read_bytes(self.length)



    class Version(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.major = self._io.read_u1()
            self.minor = self._io.read_u1()


    class HandshakeHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.msg_type = self._io.read_u1()
            self.length = self._io.read_bits_int_be(24)


    class CipherSuitesStruct(KaitaiStruct):
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



    class RecordHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content_type = self._io.read_u1()
            self.version = TlsClientHello.Version(self._io, self, self._root)
            self.length = self._io.read_u2be()


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u2be()
            self.length = self._io.read_u2be()
            self.data = self._io.read_bytes(self.length)



