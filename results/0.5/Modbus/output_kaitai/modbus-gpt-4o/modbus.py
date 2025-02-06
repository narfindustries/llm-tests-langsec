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
        _on = self._io.size()
        if _on == 8:
            self.transport = Modbus.ModbusTcp(self._io, self, self._root)
        elif _on == 6:
            self.transport = Modbus.ModbusRtu(self._io, self, self._root)

    class ReadWriteMultipleRegisters(KaitaiStruct):
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
            self.write_registers_value = self._io.read_bytes(self.write_byte_count)


    class EncapsulatedInterfaceTransport(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mei_type = self._io.read_u1()
            self.mei_data = self._io.read_bytes_full()


    class ReadDiscreteInputs(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_inputs = self._io.read_u2be()


    class ModbusRtu(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = self._io.read_u1()
            self.pdu = Modbus.Pdu(self._io, self, self._root)
            self.crc = self._io.read_u2be()


    class ReadCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()


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
            self.quantity_of_outputs = self._io.read_u2be()
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
            if _on == 4:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.ReadInputRegisters(_io__raw_data, self, self._root)
            elif _on == 6:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.WriteSingleRegister(_io__raw_data, self, self._root)
            elif _on == 1:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.ReadCoils(_io__raw_data, self, self._root)
            elif _on == 3:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.ReadHoldingRegisters(_io__raw_data, self, self._root)
            elif _on == 5:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.WriteSingleCoil(_io__raw_data, self, self._root)
            elif _on == 23:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.ReadWriteMultipleRegisters(_io__raw_data, self, self._root)
            elif _on == 15:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.WriteMultipleCoils(_io__raw_data, self, self._root)
            elif _on == 16:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.WriteMultipleRegisters(_io__raw_data, self, self._root)
            elif _on == 2:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.ReadDiscreteInputs(_io__raw_data, self, self._root)
            elif _on == 43:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.EncapsulatedInterfaceTransport(_io__raw_data, self, self._root)
            elif _on == 22:
                self._raw_data = self._io.read_bytes_full()
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Modbus.MaskWriteRegister(_io__raw_data, self, self._root)
            else:
                self.data = self._io.read_bytes_full()


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


    class ModbusTcp(KaitaiStruct):
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
            self.output_value = self._io.read_u2be()



