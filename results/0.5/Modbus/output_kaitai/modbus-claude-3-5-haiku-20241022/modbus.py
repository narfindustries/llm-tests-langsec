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
        mask_write_register = 22
        read_write_multiple_registers = 23

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        slave_device_failure = 4
        acknowledge = 5
        slave_device_busy = 6
        negative_acknowledge = 7
        memory_parity_error = 8
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        if self.is_tcp_encapsulation:
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
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_of_registers):
                self.register_values.append(self._io.read_u2be())



    class ReadWriteMultipleRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.read_starting_address = self._io.read_u2be()
            self.quantity_to_read = self._io.read_u2be()
            self.write_starting_address = self._io.read_u2be()
            self.quantity_to_write = self._io.read_u2be()
            self.write_byte_count = self._io.read_u1()
            self.write_register_values = []
            for i in range(self.quantity_to_write):
                self.write_register_values.append(self._io.read_u2be())



    class ReadInputRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()


    class WriteMultipleCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()
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


    class ProtocolDataUnit(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = KaitaiStream.resolve_enum(Modbus.FunctionCodes, self._io.read_u1())
            _on = self.function_code
            if _on == Modbus.FunctionCodes.read_write_multiple_registers:
                self.data = Modbus.ReadWriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_registers:
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
            elif _on == Modbus.FunctionCodes.mask_write_register:
                self.data = Modbus.MaskWriteRegisterRequest(self._io, self, self._root)
            else:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.coil_address = self._io.read_u2be()
            self.coil_value = self._io.read_u2be()


    class MaskWriteRegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_address = self._io.read_u2be()
            self.and_mask = self._io.read_u2be()
            self.or_mask = self._io.read_u2be()


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
            self.quantity_of_registers = self._io.read_u2be()


    class ReadCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()


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


    @property
    def is_tcp_encapsulation(self):
        """Determines if the Modbus frame is encapsulated in TCP."""
        if hasattr(self, '_m_is_tcp_encapsulation'):
            return self._m_is_tcp_encapsulation

        self._m_is_tcp_encapsulation = True
        return getattr(self, '_m_is_tcp_encapsulation', None)


