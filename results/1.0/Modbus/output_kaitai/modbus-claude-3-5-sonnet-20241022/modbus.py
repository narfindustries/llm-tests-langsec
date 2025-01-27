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
        exception_read_coils = 129
        exception_read_discrete_inputs = 130
        exception_read_holding_registers = 131
        exception_read_input_registers = 132
        exception_write_single_coil = 133
        exception_write_single_register = 134
        exception_write_multiple_coils = 143
        exception_write_multiple_registers = 144

    class ExceptionCodes(Enum):
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
        self.header = Modbus.Header(self._io, self, self._root)
        self.function_code = self._io.read_u1()
        _on = self.function_code
        if _on == 131:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 4:
            self.data = Modbus.ReadInputRegistersResponse(self._io, self, self._root)
        elif _on == 6:
            self.data = Modbus.WriteSingleRegisterResponse(self._io, self, self._root)
        elif _on == 1:
            self.data = Modbus.ReadCoilsResponse(self._io, self, self._root)
        elif _on == 144:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 143:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 3:
            self.data = Modbus.ReadHoldingRegistersResponse(self._io, self, self._root)
        elif _on == 5:
            self.data = Modbus.WriteSingleCoilResponse(self._io, self, self._root)
        elif _on == 15:
            self.data = Modbus.WriteMultipleCoilsResponse(self._io, self, self._root)
        elif _on == 130:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 133:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 129:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 16:
            self.data = Modbus.WriteMultipleRegistersResponse(self._io, self, self._root)
        elif _on == 134:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)
        elif _on == 2:
            self.data = Modbus.ReadDiscreteInputsResponse(self._io, self, self._root)
        elif _on == 132:
            self.data = Modbus.ExceptionResponse(self._io, self, self._root)

    class WriteMultipleCoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_outputs = self._io.read_u2be()


    class WriteSingleRegisterResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class ReadHoldingRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.register_value = []
            for i in range(self.byte_count // 2):
                self.register_value.append(self._io.read_u2be())



    class ReadInputRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.register_value = []
            for i in range(self.byte_count // 2):
                self.register_value.append(self._io.read_u2be())



    class ReadDiscreteInputsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.input_status = []
            for i in range(self.byte_count):
                self.input_status.append(self._io.read_u1())



    class ExceptionResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.exception_code = self._io.read_u1()


    class Header(KaitaiStruct):
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


    class ReadCoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.coil_status = []
            for i in range(self.byte_count):
                self.coil_status.append(self._io.read_u1())



    class WriteSingleCoilResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class WriteMultipleRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()



