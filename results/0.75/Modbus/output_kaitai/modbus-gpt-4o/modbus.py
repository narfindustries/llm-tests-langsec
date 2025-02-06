# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):

    class FunctionCodes(Enum):
        read_coils = 1
        read_discrete_inputs = 2
        read_holding_registers = 3
        read_input_registers = 4
        write_single_coil = 5
        write_single_register = 6
        write_multiple_coils = 15
        write_multiple_registers = 16
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
        self.function_code = self._io.read_u1()
        self.data = self._io.read_bytes((self.length - 2))

    class ModbusRtu(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unit_id = self._io.read_u1()
            self.function_code = self._io.read_u1()
            self.data = self._io.read_bytes_full()
            self.crc = self._io.read_u2be()


    class ModbusAscii(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_char = self._io.read_u1()
            self.unit_id = (self._io.read_bytes(2)).decode(u"ascii")
            self.function_code = (self._io.read_bytes(2)).decode(u"ascii")
            self.data = (self._io.read_bytes_full()).decode(u"ascii")
            self.lrc = (self._io.read_bytes(2)).decode(u"ascii")



