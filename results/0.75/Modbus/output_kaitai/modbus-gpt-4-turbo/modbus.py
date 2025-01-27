# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):
    """Modbus is a serial communications protocol originally published by Modicon (now Schneider Electric) in 1979 for use with its programmable logic controllers (PLCs). Modbus has become a de facto standard communication protocol and is now a commonly available means of connecting industrial electronic devices.
    """
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
        if _on == 1:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadCoilsResp(_io__raw_data, self, self._root)
        elif _on == 3:
            self._raw_data = self._io.read_bytes((self.length - 2))
            _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
            self.data = Modbus.ReadHoldingRegistersResp(_io__raw_data, self, self._root)
        else:
            self.data = self._io.read_bytes((self.length - 2))

    class ReadCoilsResp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.coil_status = []
            for i in range((8 * self.byte_count)):
                self.coil_status.append(self._io.read_bits_int_be(1) != 0)



    class ReadHoldingRegistersResp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.byte_count // 2):
                self.register_values.append(self._io.read_u2le())




