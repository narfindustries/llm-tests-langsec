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

    class CoilValues(Enum):
        false = 0
        true = 65280
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.mbap_header = Modbus.MbapHeader(self._io, self, self._root)
        self.pdu = Modbus.ProtocolDataUnit(self._io, self, self._root)

    class WriteMultipleRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/write_multiple_registers_request/seq/1")
            if not self.quantity_of_registers <= 123:
                raise kaitaistruct.ValidationGreaterThanError(123, self.quantity_of_registers, self._io, u"/types/write_multiple_registers_request/seq/1")
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_of_registers):
                self.register_values.append(self._io.read_u2be())



    class ReadInputRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/read_input_registers_request/seq/1")
            if not self.quantity_of_registers <= 125:
                raise kaitaistruct.ValidationGreaterThanError(125, self.quantity_of_registers, self._io, u"/types/read_input_registers_request/seq/1")


    class WriteMultipleCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()
            if not self.quantity_of_coils >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_coils, self._io, u"/types/write_multiple_coils_request/seq/1")
            if not self.quantity_of_coils <= 1968:
                raise kaitaistruct.ValidationGreaterThanError(1968, self.quantity_of_coils, self._io, u"/types/write_multiple_coils_request/seq/1")
            self.byte_count = self._io.read_u1()
            self.coil_values = []
            for i in range(self.byte_count):
                self.coil_values.append(self._io.read_u1())



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
            self.quantity_of_inputs = self._io.read_u2be()
            if not self.quantity_of_inputs >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_inputs, self._io, u"/types/read_discrete_inputs_request/seq/1")
            if not self.quantity_of_inputs <= 2000:
                raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity_of_inputs, self._io, u"/types/read_discrete_inputs_request/seq/1")


    class ProtocolDataUnit(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = KaitaiStream.resolve_enum(Modbus.FunctionCodes, self._io.read_u1())
            _on = self.function_code
            if _on == Modbus.FunctionCodes.write_multiple_registers:
                self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_register:
                self.data = Modbus.WriteSingleRegisterRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_coil:
                self.data = Modbus.WriteSingleCoilRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_holding_registers:
                self.data = Modbus.ReadHoldingRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_coils:
                self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_coils:
                self.data = Modbus.ReadCoilsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_discrete_inputs:
                self.data = Modbus.ReadDiscreteInputsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_input_registers:
                self.data = Modbus.ReadInputRegistersRequest(self._io, self, self._root)


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = KaitaiStream.resolve_enum(Modbus.CoilValues, self._io.read_u2be())


    class ReadHoldingRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/read_holding_registers_request/seq/1")
            if not self.quantity_of_registers <= 125:
                raise kaitaistruct.ValidationGreaterThanError(125, self.quantity_of_registers, self._io, u"/types/read_holding_registers_request/seq/1")


    class ReadCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()
            if not self.quantity_of_coils >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_coils, self._io, u"/types/read_coils_request/seq/1")
            if not self.quantity_of_coils <= 2000:
                raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity_of_coils, self._io, u"/types/read_coils_request/seq/1")


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



