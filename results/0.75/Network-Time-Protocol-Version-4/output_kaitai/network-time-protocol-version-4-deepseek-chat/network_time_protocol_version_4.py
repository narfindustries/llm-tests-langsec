# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NetworkTimeProtocolVersion4(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.leap_indicator = self._io.read_u2be()
        self.version = NetworkTimeProtocolVersion4.U3(self._io, self, self._root)
        self.mode = NetworkTimeProtocolVersion4.U3(self._io, self, self._root)
        self.stratum = self._io.read_u8be()
        self.poll = self._io.read_u8be()
        self.precision = self._io.read_s8be()
        self.root_delay = NetworkTimeProtocolVersion4.S32(self._io, self, self._root)
        self.root_dispersion = NetworkTimeProtocolVersion4.U32(self._io, self, self._root)
        self.reference_id = NetworkTimeProtocolVersion4.U32(self._io, self, self._root)
        self.reference_timestamp = NetworkTimeProtocolVersion4.U64(self._io, self, self._root)
        self.originate_timestamp = NetworkTimeProtocolVersion4.U64(self._io, self, self._root)
        self.receive_timestamp = NetworkTimeProtocolVersion4.U64(self._io, self, self._root)
        self.transmit_timestamp = NetworkTimeProtocolVersion4.U64(self._io, self, self._root)
        self._raw_extension_fields = self._io.read_bytes_full()
        _io__raw_extension_fields = KaitaiStream(BytesIO(self._raw_extension_fields))
        self.extension_fields = NetworkTimeProtocolVersion4.U8(_io__raw_extension_fields, self, self._root)

    class U32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(32)


    class B32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bits = self._io.read_bits_int_be(32)


    class B2(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bits = self._io.read_bits_int_be(2)


    class B3(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bits = self._io.read_bits_int_be(3)


    class B64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bits = self._io.read_bits_int_be(64)


    class S8(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(8)


    class B8(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bits = self._io.read_bits_int_be(8)


    class S32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(32)


    class U3(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(3)


    class U8(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(8)


    class U64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(64)


    class U2(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bits_int_be(2)



