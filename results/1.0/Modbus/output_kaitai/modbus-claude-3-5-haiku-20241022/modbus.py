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

    class CoilStates(Enum):
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

    class ReadDiscreteInputs(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_inputs = self._io.read_u2be()


    class ReadCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()


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
                self.payload = Modbus.WriteMultipleRegisters(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_register:
                self.payload = Modbus.WriteSingleRegister(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_coil:
                self.payload = Modbus.WriteSingleCoil(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_holding_registers:
                self.payload = Modbus.ReadHoldingRegisters(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_coils:
                self.payload = Modbus.WriteMultipleCoils(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_coils:
                self.payload = Modbus.ReadCoils(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_discrete_inputs:
                self.payload = Modbus.ReadDiscreteInputs(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_input_registers:
                self.payload = Modbus.ReadInputRegisters(self._io, self, self._root)
            else:
                self.payload = Modbus.GenericPayload(self._io, self, self._root)


    class WriteMultipleRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_of_registers):
                self.register_values.append(self._io.read_u2be())



    class WriteSingleRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class GenericPayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.raw_bytes = []
            i = 0
            while not self._io.is_eof():
                self.raw_bytes.append(self._io.read_u1())
                i += 1



    class WriteMultipleCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_outputs = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.output_values = []
            for i in range(self.quantity_of_outputs):
                self.output_values.append(self._io.read_bits_int_be(1) != 0)



    class ReadHoldingRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()


    class ReadInputRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()


    class WriteSingleCoil(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = KaitaiStream.resolve_enum(Modbus.CoilStates, self._io.read_u2be())


    class MbapHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transaction_identifier = self._io.read_u2be()
            self.protocol_identifier = self._io.read_u2be()
            if not self.protocol_identifier == 0:
                raise kaitaistruct.ValidationNotEqualError(0, self.protocol_identifier, self._io, u"/types/mbap_header/seq/1")
            self.length = self._io.read_u2be()
            self.unit_identifier = self._io.read_u1()



