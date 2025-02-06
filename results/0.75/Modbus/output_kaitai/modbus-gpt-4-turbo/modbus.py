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
        self.transaction_id = self._io.read_u2le()
        self.protocol_id = self._io.read_u2le()
        self.length = self._io.read_u2le()
        self.unit_id = self._io.read_u1()
        self.function_code = self._io.read_u1()
        _on = self.function_code
        if _on == 4:
            self.data = Modbus.InputRegistersResponse(self._io, self, self._root)
        elif _on == 6:
            self.data = Modbus.SingleRegisterResponse(self._io, self, self._root)
        elif _on == 1:
            self.data = Modbus.CoilsResponse(self._io, self, self._root)
        elif _on == 3:
            self.data = Modbus.HoldingRegistersResponse(self._io, self, self._root)
        elif _on == 5:
            self.data = Modbus.SingleCoilResponse(self._io, self, self._root)
        elif _on == 15:
            self.data = Modbus.MultipleCoilsResponse(self._io, self, self._root)
        elif _on == 16:
            self.data = Modbus.MultipleRegistersResponse(self._io, self, self._root)
        elif _on == 2:
            self.data = Modbus.DiscreteInputsResponse(self._io, self, self._root)

    class DiscreteInputsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.input_status = []
            for i in range((self.byte_count * 8)):
                self.input_status.append(self._io.read_bits_int_be(1) != 0)



    class HoldingRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.registers = []
            for i in range(self.byte_count // 2):
                self.registers.append(self._io.read_u2le())



    class MultipleCoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2le()
            self.quantity_of_outputs = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.output_status = []
            for i in range((self.byte_count * 8)):
                self.output_status.append(self._io.read_bits_int_be(1) != 0)



    class SingleRegisterResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2le()
            self.register_value = self._io.read_u2le()


    class MultipleRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2le()
            self.quantity_of_registers = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.byte_count // 2):
                self.register_values.append(self._io.read_u2le())



    class SingleCoilResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2le()
            self.output_value = self._io.read_u2le()


    class CoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.coil_status = []
            for i in range((self.byte_count * 8)):
                self.coil_status.append(self._io.read_bits_int_be(1) != 0)



    class InputRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.input_registers = []
            for i in range(self.byte_count // 2):
                self.input_registers.append(self._io.read_u2le())




