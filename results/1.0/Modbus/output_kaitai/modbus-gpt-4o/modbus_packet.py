# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ModbusPacket(KaitaiStruct):
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
        if _on == 3:
            self._raw_payload = self._io.read_bytes((self.length - 2))
            _io__raw_payload = KaitaiStream(BytesIO(self._raw_payload))
            self.payload = ModbusPacket.FunctionReadHoldingRegistersResponse(_io__raw_payload, self, self._root)
        elif _on == 6:
            self._raw_payload = self._io.read_bytes((self.length - 2))
            _io__raw_payload = KaitaiStream(BytesIO(self._raw_payload))
            self.payload = ModbusPacket.FunctionPresetSingleRegisterRequest(_io__raw_payload, self, self._root)
        else:
            self.payload = self._io.read_bytes((self.length - 2))

    class FunctionReadHoldingRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.byte_count // 2):
                self.register_values.append(self._io.read_u2be())



    class FunctionPresetSingleRegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = self._io.read_u2be()
            self.value = self._io.read_u2be()



