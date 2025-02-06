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
        slave_device_failure = 4
        acknowledge = 5
        slave_device_busy = 6
        memory_parity_error = 8
        gateway_path_unavailable = 10
        gateway_target_device_failed_to_respond = 11
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        if self._io.size() >= 7:
            self.mbap_header = Modbus.MbapHeader(self._io, self, self._root)

        self.pdu = Modbus.Pdu(self._io, self, self._root)

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
            self.register_values = self._io.read_bytes(self.byte_count)


    class ErrorResponse(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.exception_code = KaitaiStream.resolve_enum(Modbus.ExceptionCodes, self._io.read_u1())


    class ReadWriteMultipleRegistersRequest(KaitaiStruct):
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
            self.write_registers = self._io.read_bytes(self.write_byte_count)


    class ReadFileRecordRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.subrequests = []
            for i in range(self.byte_count // 7):
                self.subrequests.append(Modbus.FileRecordRequest(self._io, self, self._root))



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
            self.coil_values = self._io.read_bytes(self.byte_count)


    class WriteSingleRegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.register_address = self._io.read_u2be()
            self.register_value = self._io.read_u2be()


    class ReadFifoQueueRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fifo_pointer_address = self._io.read_u2be()


    class ReadDiscreteInputsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = self._io.read_u2be()


    class FileRecordWriteRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_type = self._io.read_u1()
            self.file_number = self._io.read_u2be()
            self.record_number = self._io.read_u2be()
            self.record_length = self._io.read_u2be()
            self.record_data = self._io.read_bytes((self.record_length * 2))


    class MaskWriteRegisterRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_address = self._io.read_u2be()
            self.and_mask = self._io.read_u2be()
            self.or_mask = self._io.read_u2be()


    class Pdu(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = self._io.read_u1()
            _on = self.function_code
            if _on == 131:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 4:
                self.data = Modbus.ReadInputRegistersRequest(self._io, self, self._root)
            elif _on == 24:
                self.data = Modbus.ReadFifoQueueRequest(self._io, self, self._root)
            elif _on == 6:
                self.data = Modbus.WriteSingleRegisterRequest(self._io, self, self._root)
            elif _on == 20:
                self.data = Modbus.ReadFileRecordRequest(self._io, self, self._root)
            elif _on == 1:
                self.data = Modbus.ReadCoilsRequest(self._io, self, self._root)
            elif _on == 150:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 144:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 149:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 143:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 3:
                self.data = Modbus.ReadHoldingRegistersRequest(self._io, self, self._root)
            elif _on == 5:
                self.data = Modbus.WriteSingleCoilRequest(self._io, self, self._root)
            elif _on == 23:
                self.data = Modbus.ReadWriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == 15:
                self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
            elif _on == 148:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 152:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 130:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 21:
                self.data = Modbus.WriteFileRecordRequest(self._io, self, self._root)
            elif _on == 133:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 129:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 151:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 16:
                self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == 134:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 2:
                self.data = Modbus.ReadDiscreteInputsRequest(self._io, self, self._root)
            elif _on == 132:
                self.data = Modbus.ErrorResponse(self._io, self, self._root)
            elif _on == 22:
                self.data = Modbus.MaskWriteRegisterRequest(self._io, self, self._root)


    class WriteFileRecordRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.subrequests = []
            i = 0
            while not self._io.is_eof():
                self.subrequests.append(Modbus.FileRecordWriteRequest(self._io, self, self._root))
                i += 1



    class ReadHoldingRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity = self._io.read_u2be()


    class FileRecordRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_type = self._io.read_u1()
            self.file_number = self._io.read_u2be()
            self.record_number = self._io.read_u2be()
            self.record_length = self._io.read_u2be()


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



