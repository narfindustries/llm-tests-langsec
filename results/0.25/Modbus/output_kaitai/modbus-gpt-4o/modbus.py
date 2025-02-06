# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2be()
        self.protocol_id = self._io.read_u2be()
        self.length = self._io.read_u2be()
        self.unit_id = self._io.read_u1()
        self.address = self._io.read_u1()
        self.function_code = self._io.read_u1()
        self._raw_data = self._io.read_bytes((self.length - 2))
        _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
        self.data = Modbus.DataBlock(_io__raw_data, self, self._root)
        self.error_check = self._io.read_u2be()

    class DataBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()
            self.values = self._io.read_bytes_full()



