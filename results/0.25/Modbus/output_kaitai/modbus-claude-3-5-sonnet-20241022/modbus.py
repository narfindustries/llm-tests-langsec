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
        read_coils_error = 129
        read_discrete_inputs_error = 130
        read_holding_registers_error = 131
        read_input_registers_error = 132
        write_single_coil_error = 133
        write_single_register_error = 134
        write_multiple_coils_error = 143
        write_multiple_registers_error = 144

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        server_device_failure = 4
        acknowledge = 5
        server_device_busy = 6
        memory_parity_error = 8
        gateway_path_unavailable = 10
        gateway_target_device_failed = 11
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        if self._io.size() >= 7:
            self.mbap_header = Modbus.MbapHeader(self._io, self, self._root)

        self.pdu = Modbus.Pdu(self._io, self, self._root)

    class WriteMultipleRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.register_values = self._io.read_bytes(self.byte_count)


    class ReadInputRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteMultipleCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.coil_values = self._io.read_bytes(self.byte_count)


    class WriteSingleRegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class ReadDiscreteInputsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class Pdu(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = self._io.read_u1()
            _on = self.function_code
            if _on == 131:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 4:
                self.data = Modbus.ReadInputRegistersRequest(self._io, self, self._root)
            elif _on == 6:
                self.data = Modbus.WriteSingleRegisterRequest(self._io, self, self._root)
            elif _on == 1:
                self.data = Modbus.ReadCoilsRequest(self._io, self, self._root)
            elif _on == 144:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 143:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 3:
                self.data = Modbus.ReadHoldingRegistersRequest(self._io, self, self._root)
            elif _on == 5:
                self.data = Modbus.WriteSingleCoilRequest(self._io, self, self._root)
            elif _on == 15:
                self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
            elif _on == 130:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 133:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 129:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 16:
                self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == 134:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 2:
                self.data = Modbus.ReadDiscreteInputsRequest(self._io, self, self._root)
            elif _on == 132:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)


    class ExceptionResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.exception_code = KaitaiStream.resolve_enum(Modbus.ExceptionCodes, self._io.read_u1())


    class ReadHoldingRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class ReadCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class MbapHeader(KaitaiStruct):
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



