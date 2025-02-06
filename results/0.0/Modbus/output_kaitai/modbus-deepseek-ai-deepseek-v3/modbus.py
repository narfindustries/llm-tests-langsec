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
        self.function_code = self._io.read_u1()
        _on = self.function_code
        if _on == 17:
            self.data = Modbus.ReportServerId(self._io, self, self._root)
        elif _on == 4:
            self.data = Modbus.ReadInputRegisters(self._io, self, self._root)
        elif _on == 24:
            self.data = Modbus.ReadFifoQueue(self._io, self, self._root)
        elif _on == 6:
            self.data = Modbus.WriteSingleRegister(self._io, self, self._root)
        elif _on == 20:
            self.data = Modbus.ReadFileRecord(self._io, self, self._root)
        elif _on == 7:
            self.data = Modbus.ReadExceptionStatus(self._io, self, self._root)
        elif _on == 1:
            self.data = Modbus.ReadCoils(self._io, self, self._root)
        elif _on == 11:
            self.data = Modbus.GetCommEventCounter(self._io, self, self._root)
        elif _on == 12:
            self.data = Modbus.GetCommEventLog(self._io, self, self._root)
        elif _on == 3:
            self.data = Modbus.ReadHoldingRegisters(self._io, self, self._root)
        elif _on == 5:
            self.data = Modbus.WriteSingleCoil(self._io, self, self._root)
        elif _on == 23:
            self.data = Modbus.ReadWriteMultipleRegisters(self._io, self, self._root)
        elif _on == 15:
            self.data = Modbus.WriteMultipleCoils(self._io, self, self._root)
        elif _on == 8:
            self.data = Modbus.Diagnostics(self._io, self, self._root)
        elif _on == 21:
            self.data = Modbus.WriteFileRecord(self._io, self, self._root)
        elif _on == 16:
            self.data = Modbus.WriteMultipleRegisters(self._io, self, self._root)
        elif _on == 2:
            self.data = Modbus.ReadDiscreteInputs(self._io, self, self._root)
        elif _on == 43:
            self.data = Modbus.EncapsulatedInterfaceTransport(self._io, self, self._root)
        elif _on == 22:
            self.data = Modbus.MaskWriteRegister(self._io, self, self._root)
        if self.function_code >= 128:
            self.error_code = self._io.read_u1()

        if self.function_code >= 128:
            self.exception_code = self._io.read_u1()

        self.checksum = self._io.read_u2be()

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
            self.write_registers = (self._io.read_bytes(self.write_byte_count)).decode(u"ASCII")


    class ReadFileRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.file_records = (self._io.read_bytes(self.byte_count)).decode(u"ASCII")


    class EncapsulatedInterfaceTransport(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mei_type = self._io.read_u1()
            self.mei_data = (self._io.read_bytes_full()).decode(u"ASCII")


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
            self.registers = (self._io.read_bytes(self.byte_count)).decode(u"ASCII")


    class WriteSingleRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class ReportServerId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class GetCommEventCounter(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


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
            self.coils = (self._io.read_bytes(self.byte_count)).decode(u"ASCII")


    class Diagnostics(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sub_function = self._io.read_u2be()
            self.data = self._io.read_u2be()


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


    class ReadExceptionStatus(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class WriteFileRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.file_records = (self._io.read_bytes(self.byte_count)).decode(u"ASCII")


    class WriteSingleCoil(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class GetCommEventLog(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass



