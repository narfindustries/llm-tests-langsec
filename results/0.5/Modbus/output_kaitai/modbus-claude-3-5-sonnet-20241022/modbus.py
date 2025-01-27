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

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        server_device_failure = 4
        acknowledge = 5
        server_device_busy = 6
        memory_parity_error = 8
        gateway_path_unavailable = 10
        gateway_target_device_failed_to_respond = 11
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
        self.function_code = KaitaiStream.resolve_enum(Modbus.FunctionCodes, self._io.read_u1())
        _on = self.function_code
        if _on == Modbus.FunctionCodes.write_multiple_registers:
            self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.write_single_register:
            self.data = Modbus.WriteSingleRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.write_single_coil:
            self.data = Modbus.WriteSingleRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.read_holding_registers:
            self.data = Modbus.ReadRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.write_multiple_coils:
            self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.read_coils:
            self.data = Modbus.ReadRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.read_discrete_inputs:
            self.data = Modbus.ReadRequest(self._io, self, self._root)
        elif _on == Modbus.FunctionCodes.read_input_registers:
            self.data = Modbus.ReadRequest(self._io, self, self._root)
        else:
            self.data = Modbus.RawData(self._io, self, self._root)

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


    class WriteSingleRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class RawData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bytes = self._io.read_bytes_full()


    class ReadRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()



