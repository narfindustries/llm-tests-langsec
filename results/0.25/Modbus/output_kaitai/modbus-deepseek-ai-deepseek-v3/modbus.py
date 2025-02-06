# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        slave_device_failure = 4
        acknowledge = 5
        slave_device_busy = 6
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
        self.transaction_id = self._io.read_u2be()
        self.protocol_id = self._io.read_u2be()
        self.length = self._io.read_u2be()
        self.unit_id = self._io.read_u1()
        self.function_code = self._io.read_u1()
        _on = self.function_code
        if _on == 4:
            self.data = Modbus.ReadInputRegisters(self._io, self, self._root)
        elif _on == 24:
            self.data = Modbus.ReadFifoQueue(self._io, self, self._root)
        elif _on == 6:
            self.data = Modbus.WriteSingleRegister(self._io, self, self._root)
        elif _on == 1:
            self.data = Modbus.ReadCoils(self._io, self, self._root)
        elif _on == 3:
            self.data = Modbus.ReadHoldingRegisters(self._io, self, self._root)
        elif _on == 5:
            self.data = Modbus.WriteSingleCoil(self._io, self, self._root)
        elif _on == 23:
            self.data = Modbus.ReadWriteMultipleRegisters(self._io, self, self._root)
        elif _on == 15:
            self.data = Modbus.WriteMultipleCoils(self._io, self, self._root)
        elif _on == 16:
            self.data = Modbus.WriteMultipleRegisters(self._io, self, self._root)
        elif _on == 2:
            self.data = Modbus.ReadDiscreteInputs(self._io, self, self._root)
        elif _on == 43:
            self.data = Modbus.EncapsulatedInterfaceTransport(self._io, self, self._root)
        elif _on == 22:
            self.data = Modbus.MaskWriteRegister(self._io, self, self._root)

    class ReadFifoQueue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fifo_pointer_address = self._io.read_u2be()


    class ReadWriteMultipleRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.read_starting_address = self._io.read_u2be()
            self.read_quantity = self._io.read_u2be()
            self.write_starting_address = self._io.read_u2be()
            self.num_write_registers = self._io.read_u2be()
            self.write_byte_count = self._io.read_u1()
            self.write_registers = []
            for i in range(self.num_write_registers):
                self.write_registers.append(self._io.read_u2be())



    class EncapsulatedInterfaceTransport(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mei_type = self._io.read_u1()
            self.device_id_code = self._io.read_u1()
            self.object_id = self._io.read_u1()
            self.object_value = self._io.read_u1()


    class ReadDiscreteInputs(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class ReadCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteMultipleRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.num_registers = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.registers = []
            for i in range(self.num_registers):
                self.registers.append(self._io.read_u2be())



    class WriteSingleRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class WriteMultipleCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.num_coils = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.coils = []
            for i in range(self.byte_count):
                self.coils.append(self._io.read_u1())



    class MaskWriteRegister(KaitaiStruct):
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
            self.exception_function_code = self._io.read_u1()
            self.exception_code = KaitaiStream.resolve_enum(Modbus.ExceptionCodes, self._io.read_u1())


    class ReadHoldingRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class ReadInputRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteSingleCoil(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()



