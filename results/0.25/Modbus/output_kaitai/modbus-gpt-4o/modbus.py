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
        if _on == 4:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadInputRegistersRequest(_io__raw_data, self, self._root)
        elif _on == 6:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.WriteSingleRegisterRequest(_io__raw_data, self, self._root)
        elif _on == 1:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadCoilsRequest(_io__raw_data, self, self._root)
        elif _on == 3:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadHoldingRegistersRequest(_io__raw_data, self, self._root)
        elif _on == 5:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.WriteSingleCoilRequest(_io__raw_data, self, self._root)
        elif _on == 15:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.WriteMultipleCoilsRequest(_io__raw_data, self, self._root)
        elif _on == 16:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.WriteMultipleRegistersRequest(_io__raw_data, self, self._root)
        elif _on == 2:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadDiscreteInputsRequest(_io__raw_data, self, self._root)
        else:
            self.data = self._io.read_bytes((self.length - 2))

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
            self.register_values = self._io.read_bytes(self.byte_count)


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
            self.quantity_of_outputs = self._io.read_u2be()
            self.byte_count = self._io.read_u1()
            self.output_values = self._io.read_bytes(self.byte_count)


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


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


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



