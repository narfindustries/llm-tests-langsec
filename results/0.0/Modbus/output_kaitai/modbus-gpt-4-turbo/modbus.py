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
            self.data = Modbus.RegisterRequest(self._io, self, self._root)
        elif _on == 6:
            self.data = Modbus.SingleRegister(self._io, self, self._root)
        elif _on == 1:
            self.data = Modbus.CoilsRequest(self._io, self, self._root)
        elif _on == 3:
            self.data = Modbus.RegisterRequest(self._io, self, self._root)
        elif _on == 5:
            self.data = Modbus.SingleCoil(self._io, self, self._root)
        elif _on == 15:
            self.data = Modbus.MultipleCoils(self._io, self, self._root)
        elif _on == 16:
            self.data = Modbus.MultipleRegisters(self._io, self, self._root)
        elif _on == 2:
            self.data = Modbus.CoilsRequest(self._io, self, self._root)

    class MultipleCoils(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_addr = self._io.read_u2le()
            self.quantity_outputs = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.output_values = []
            for i in range(self.quantity_outputs):
                self.output_values.append(self._io.read_bits_int_be(1) != 0)



    class CoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_addr = self._io.read_u2le()
            self.quantity = self._io.read_u2le()


    class SingleRegister(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_addr = self._io.read_u2le()
            self.register_value = self._io.read_u2le()


    class RegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_addr = self._io.read_u2le()
            self.quantity = self._io.read_u2le()


    class MultipleRegisters(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.start_addr = self._io.read_u2le()
            self.quantity_regs = self._io.read_u2le()
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_regs):
                self.register_values.append(self._io.read_u2le())



    class SingleCoil(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_addr = self._io.read_u2le()
            self.output_value = self._io.read_u2le()



