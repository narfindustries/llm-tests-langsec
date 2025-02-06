# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):

    class ExceptionCodes(Enum):
        illegal_function = 1
        illegal_data_address = 2
        illegal_data_value = 3
        server_device_failure = 4
        acknowledge = 5
        server_device_busy = 6
        memory_parity_error = 8
        gateway_path_unavailable = 10
        gateway_target_device_failed = 11
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.mbap = Modbus.MbapHeader(self._io, self, self._root)
        self._raw_pdu = self._io.read_bytes((self.mbap.length - 1))
        _io__raw_pdu = KaitaiStream(BytesIO(self._raw_pdu))
        self.pdu = Modbus.ProtocolDataUnit(_io__raw_pdu, self, self._root)

    class WriteMultipleRegistersRequest(KaitaiStruct):
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


    class WriteMultipleCoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class ReadInputRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteMultipleCoilsRequest(KaitaiStruct):
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
            self.quantity = self._io.read_u2be()


    class ProtocolDataUnit(KaitaiStruct):
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
            elif _on == 4:
                self.data = Modbus.ReadInputRegistersRequest(self._io, self, self._root)
            elif _on == 6:
                self.data = Modbus.WriteSingleRegisterRequest(self._io, self, self._root)
            elif _on == 1:
                self.data = Modbus.ReadCoilsRequest(self._io, self, self._root)
            elif _on == 144:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 143:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 3:
                self.data = Modbus.ReadHoldingRegistersRequest(self._io, self, self._root)
            elif _on == 5:
                self.data = Modbus.WriteSingleCoilRequest(self._io, self, self._root)
            elif _on == 15:
                self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
            elif _on == 130:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 133:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 129:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 16:
                self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == 134:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)
            elif _on == 2:
                self.data = Modbus.ReadDiscreteInputsRequest(self._io, self, self._root)
            elif _on == 132:
                self.data = Modbus.ExceptionResponse(self._io, self, self._root)


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class ReadHoldingRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.register_value = self._io.read_bytes(self.byte_count)


    class ReadInputRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.input_registers = self._io.read_bytes(self.byte_count)


    class ReadDiscreteInputsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.input_status = self._io.read_bytes(self.byte_count)


    class ExceptionResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.exception_code = KaitaiStream.resolve_enum(Modbus.ExceptionCodes, self._io.read_u1())


    class ReadCoilsResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.coil_status = self._io.read_bytes(self.byte_count)


    class ReadHoldingRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class ReadCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class MbapHeader(KaitaiStruct):
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


    class WriteMultipleRegistersResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()



