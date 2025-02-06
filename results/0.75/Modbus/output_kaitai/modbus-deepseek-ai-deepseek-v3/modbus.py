# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):
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
        self.pdu = Modbus.Pdu(self._io, self, self._root)

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
            self.write_quantity = self._io.read_u2be()
            self.write_byte_count = self._io.read_u1()
            self.write_registers_value = self._io.read_bytes(self.write_byte_count)


    class EncapsulatedInterfaceTransport(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mei_type = self._io.read_u1()
            self.mei_data = self._io.read_u1()


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
            self.quantity = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.registers_value = self._io.read_bytes(self.byte_count)


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
            self.quantity = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.outputs_value = self._io.read_bytes(self.byte_count)


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
            elif _on == 136:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 4:
                self.data = Modbus.ReadInputRegisters(self._io, self, self._root)
            elif _on == 24:
                self.data = Modbus.ReadFifoQueue(self._io, self, self._root)
            elif _on == 6:
                self.data = Modbus.WriteSingleRegister(self._io, self, self._root)
            elif _on == 1:
                self.data = Modbus.ReadCoils(self._io, self, self._root)
            elif _on == 138:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 3:
                self.data = Modbus.ReadHoldingRegisters(self._io, self, self._root)
            elif _on == 5:
                self.data = Modbus.WriteSingleCoil(self._io, self, self._root)
            elif _on == 23:
                self.data = Modbus.ReadWriteMultipleRegisters(self._io, self, self._root)
            elif _on == 15:
                self.data = Modbus.WriteMultipleCoils(self._io, self, self._root)
            elif _on == 130:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 133:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 129:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 16:
                self.data = Modbus.WriteMultipleRegisters(self._io, self, self._root)
            elif _on == 134:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 139:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 2:
                self.data = Modbus.ReadDiscreteInputs(self._io, self, self._root)
            elif _on == 135:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 132:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 43:
                self.data = Modbus.EncapsulatedInterfaceTransport(self._io, self, self._root)
            elif _on == 22:
                self.data = Modbus.MaskWriteRegister(self._io, self, self._root)


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
            self.exception_code = self._io.read_u1()


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



