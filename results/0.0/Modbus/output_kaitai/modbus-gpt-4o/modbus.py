# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):

    class FunctionCode(Enum):
        read_coils = 1
        read_discrete_inputs = 2
        read_holding_registers = 3
        read_input_registers = 4
        write_single_coil = 5
        write_single_register = 6
        write_multiple_coils = 15
        write_multiple_registers = 16
        exception_read_coils = 129
        exception_read_discrete_inputs = 130
        exception_read_holding_registers = 131
        exception_read_input_registers = 132
        exception_write_single_coil = 133
        exception_write_single_register = 134
        exception_write_multiple_coils = 143
        exception_write_multiple_registers = 144

    class ExceptionCode(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        slave_device_failure = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.slave_address = self._io.read_u1()
        self.function_code = self._io.read_u1()
        self._raw_data = self._io.read_bytes_full()
        _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
        self.data = Modbus.DataField(_io__raw_data, self, self._root)
        self.crc = self._io.read_u2be()

    class DataField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()
            self.values = self._io.read_bytes_full()



