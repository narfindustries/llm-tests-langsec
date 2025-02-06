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

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        server_device_failure = 4
        acknowledge = 5
        server_device_busy = 6
        negative_acknowledge = 7
        memory_parity_error = 8
        gateway_path_unavailable = 10
        gateway_target_device_failed_to_respond = 11
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.mbap_header = Modbus.MbapHeader(self._io, self, self._root)
        self.pdu = Modbus.Pdu(self._io, self, self._root)

    class MbapHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transaction_id = self._io.read_u2be()
            self.protocol_id = self._io.read_u2be()
            if not self.protocol_id == 0:
                raise kaitaistruct.ValidationNotEqualError(0, self.protocol_id, self._io, u"/types/mbap_header/seq/1")
            self.length = self._io.read_u2be()
            self.unit_id = self._io.read_u1()


    class Pdu(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = KaitaiStream.resolve_enum(Modbus.FunctionCodes, self._io.read_u1())

        class ReadDiscreteInputs(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 2000:
                    raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity, self._io, u"/types/pdu/types/read_discrete_inputs/seq/1")


        class ReadCoils(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 2000:
                    raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity, self._io, u"/types/pdu/types/read_coils/seq/1")


        class WriteMultipleRegisters(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 123:
                    raise kaitaistruct.ValidationGreaterThanError(123, self.quantity, self._io, u"/types/pdu/types/write_multiple_registers/seq/1")
                self.byte_count = self._io.read_u1()
                self.register_values = []
                for i in range(self.quantity):
                    self.register_values.append(self._io.read_u2be())



        class WriteSingleRegister(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.address = self._io.read_u2be()
                self.value = self._io.read_u2be()


        class WriteMultipleCoils(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 1968:
                    raise kaitaistruct.ValidationGreaterThanError(1968, self.quantity, self._io, u"/types/pdu/types/write_multiple_coils/seq/1")
                self.byte_count = self._io.read_u1()
                self.coil_values = []
                for i in range(self.byte_count):
                    self.coil_values.append(self._io.read_u1())



        class ReadHoldingRegisters(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 125:
                    raise kaitaistruct.ValidationGreaterThanError(125, self.quantity, self._io, u"/types/pdu/types/read_holding_registers/seq/1")


        class ReadInputRegisters(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.start_address = self._io.read_u2be()
                self.quantity = self._io.read_u2be()
                if not self.quantity <= 125:
                    raise kaitaistruct.ValidationGreaterThanError(125, self.quantity, self._io, u"/types/pdu/types/read_input_registers/seq/1")


        class WriteSingleCoil(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.address = self._io.read_u2be()
                self.value = KaitaiStream.resolve_enum(Modbus.CoilValues, self._io.read_u2be())


        @property
        def request_data(self):
            if hasattr(self, '_m_request_data'):
                return self._m_request_data

            io = self._root._io
            _pos = io.pos()
            io.seek(self._io.pos())
            _on = self.function_code
            if _on == Modbus.FunctionCodes.write_multiple_registers:
                self._m_request_data = Modbus.Pdu.WriteMultipleRegisters(io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_register:
                self._m_request_data = Modbus.Pdu.WriteSingleRegister(io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_coil:
                self._m_request_data = Modbus.Pdu.WriteSingleCoil(io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_holding_registers:
                self._m_request_data = Modbus.Pdu.ReadHoldingRegisters(io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_coils:
                self._m_request_data = Modbus.Pdu.WriteMultipleCoils(io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_coils:
                self._m_request_data = Modbus.Pdu.ReadCoils(io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_discrete_inputs:
                self._m_request_data = Modbus.Pdu.ReadDiscreteInputs(io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_input_registers:
                self._m_request_data = Modbus.Pdu.ReadInputRegisters(io, self, self._root)
            io.seek(_pos)
            return getattr(self, '_m_request_data', None)


    class ExceptionResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = self._io.read_u1()
            self.exception_code = KaitaiStream.resolve_enum(Modbus.ExceptionCodes, self._io.read_u1())



