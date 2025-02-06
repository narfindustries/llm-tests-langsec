# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ModbusFrame(KaitaiStruct):
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
            self.data = ModbusFrame.InputRegister(self._io, self, self._root)
        elif _on == 6:
            self.data = ModbusFrame.SingleRegister(self._io, self, self._root)
        elif _on == 1:
            self.data = ModbusFrame.CoilsStatus(self._io, self, self._root)
        elif _on == 3:
            self.data = ModbusFrame.HoldingRegister(self._io, self, self._root)
        elif _on == 5:
            self.data = ModbusFrame.SingleCoil(self._io, self, self._root)
        elif _on == 15:
            self.data = ModbusFrame.MultipleCoils(self._io, self, self._root)
        elif _on == 16:
            self.data = ModbusFrame.MultipleRegisters(self._io, self, self._root)
        elif _on == 2:
            self.data = ModbusFrame.InputStatus(self._io, self, self._root)

    class MultipleCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.num_values = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.values = []
            for i in range(self.num_values):
                self.values.append(self._io.read_bits_int_be(1) != 0)



    class InputStatus(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.status = []
            for i in range(((self._parent.length - 1) * 8)):
                self.status.append(self._io.read_bits_int_be(1) != 0)



    class HoldingRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.values = []
            for i in range((self._parent.length - 1) // 2):
                self.values.append(self._io.read_u2be())



    class SingleRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class MultipleRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.num_register_values = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.num_register_values):
                self.register_values.append(self._io.read_u2be())



    class InputRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.values = []
            for i in range((self._parent.length - 1) // 2):
                self.values.append(self._io.read_u2be())



    class CoilsStatus(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.status = []
            for i in range(((self._parent.length - 1) * 8)):
                self.status.append(self._io.read_bits_int_be(1) != 0)



    class SingleCoil(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()



