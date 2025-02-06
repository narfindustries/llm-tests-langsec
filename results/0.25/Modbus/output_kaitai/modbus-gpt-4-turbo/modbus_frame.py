# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ModbusFrame(KaitaiStruct):

    class CoilValue(Enum):
        false = 0
        true = 65280
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2le()
        self.protocol_id = self._io.read_u2le()
        self.length = self._io.read_u2le()
        self.unit_id = self._io.read_u1()
        self.function_code = self._io.read_u1()
        _on = self.function_code
        if _on == 4:
            self.data = ModbusFrame.InputRegistersReq(self._io, self, self._root)
        elif _on == 6:
            self.data = ModbusFrame.WriteSingleRegisterReq(self._io, self, self._root)
        elif _on == 1:
            self.data = ModbusFrame.CoilsReq(self._io, self, self._root)
        elif _on == 3:
            self.data = ModbusFrame.HoldingRegistersReq(self._io, self, self._root)
        elif _on == 5:
            self.data = ModbusFrame.WriteSingleCoilReq(self._io, self, self._root)
        elif _on == 15:
            self.data = ModbusFrame.WriteMultipleCoilsReq(self._io, self, self._root)
        elif _on == 16:
            self.data = ModbusFrame.WriteMultipleRegistersReq(self._io, self, self._root)
        elif _on == 2:
            self.data = ModbusFrame.DiscreteInputsReq(self._io, self, self._root)

    class WriteMultipleCoilsReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_outputs = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.output_values = []
            for i in range(self.quantity_of_outputs):
                self.output_values.append(self._io.read_bits_int_be(1) != 0)



    class CoilsReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_coils = self._io.read_u2le()


    class InputRegistersReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_registers = self._io.read_u2le()


    class WriteSingleCoilReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_addr = self._io.read_u2le()
            self.output_value = KaitaiStream.resolve_enum(ModbusFrame.CoilValue, self._io.read_u2le())


    class DiscreteInputsReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_inputs = self._io.read_u2le()


    class WriteSingleRegisterReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_addr = self._io.read_u2le()
            self.register_value = self._io.read_u2le()


    class WriteMultipleRegistersReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_registers = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_of_registers):
                self.register_values.append(self._io.read_u2le())



    class HoldingRegistersReq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_addr = self._io.read_u2le()
            self.quantity_of_registers = self._io.read_u2le()



