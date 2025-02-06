# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Modbus(KaitaiStruct):

    class FunctionCodes(Enum):
        read_coils = 1
        read_discrete_inputs = 2
        read_holding_registers = 3
        read_input_registers = 4
        write_single_coil = 5
        write_single_register = 6
        write_multiple_coils = 15
        write_multiple_registers = 16
        read_file_record = 20
        write_file_record = 21
        mask_write_register = 22
        read_write_multiple_registers = 23
        read_device_identification = 43

    class CoilStates(Enum):
        false = 0
        true = 65280

    class ReferenceTypes(Enum):
        request_record_access = 6

    class MeiTypes(Enum):
        read_device_identification = 14
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.mbap_header = Modbus.MbapHeader(self._io, self, self._root)
        self.pdu = Modbus.ProtocolDataUnit(self._io, self, self._root)

    class WriteMultipleRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/write_multiple_registers_request/seq/1")
            if not self.quantity_of_registers <= 123:
                raise kaitaistruct.ValidationGreaterThanError(123, self.quantity_of_registers, self._io, u"/types/write_multiple_registers_request/seq/1")
            self.byte_count = self._io.read_u1()
            self.register_values = []
            for i in range(self.quantity_of_registers):
                self.register_values.append(self._io.read_u2be())



    class ReadWriteMultipleRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.read_starting_address = self._io.read_u2be()
            self.quantity_to_read = self._io.read_u2be()
            if not self.quantity_to_read >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_to_read, self._io, u"/types/read_write_multiple_registers_request/seq/1")
            if not self.quantity_to_read <= 125:
                raise kaitaistruct.ValidationGreaterThanError(125, self.quantity_to_read, self._io, u"/types/read_write_multiple_registers_request/seq/1")
            self.write_starting_address = self._io.read_u2be()
            self.quantity_to_write = self._io.read_u2be()
            if not self.quantity_to_write >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_to_write, self._io, u"/types/read_write_multiple_registers_request/seq/3")
            if not self.quantity_to_write <= 123:
                raise kaitaistruct.ValidationGreaterThanError(123, self.quantity_to_write, self._io, u"/types/read_write_multiple_registers_request/seq/3")
            self.write_byte_count = self._io.read_u1()
            self.write_register_values = []
            for i in range(self.quantity_to_write):
                self.write_register_values.append(self._io.read_u2be())



    class ReadFileRecordRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.records = []
            for i in range((self.byte_count - 1) // 7):
                self.records.append(Modbus.FileRecord(self._io, self, self._root))



    class ReadInputRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/read_input_registers_request/seq/1")
            if not self.quantity_of_registers <= 125:
                raise kaitaistruct.ValidationGreaterThanError(125, self.quantity_of_registers, self._io, u"/types/read_input_registers_request/seq/1")


    class WriteMultipleCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()
            if not self.quantity_of_coils >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_coils, self._io, u"/types/write_multiple_coils_request/seq/1")
            if not self.quantity_of_coils <= 1968:
                raise kaitaistruct.ValidationGreaterThanError(1968, self.quantity_of_coils, self._io, u"/types/write_multiple_coils_request/seq/1")
            self.byte_count = self._io.read_u1()
            self.coil_values = []
            for i in range(self.byte_count):
                self.coil_values.append(self._io.read_u1())



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
            if not self.quantity_of_inputs >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_inputs, self._io, u"/types/read_discrete_inputs_request/seq/1")
            if not self.quantity_of_inputs <= 2000:
                raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity_of_inputs, self._io, u"/types/read_discrete_inputs_request/seq/1")


    class ProtocolDataUnit(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.function_code = KaitaiStream.resolve_enum(Modbus.FunctionCodes, self._io.read_u1())
            _on = self.function_code
            if _on == Modbus.FunctionCodes.read_write_multiple_registers:
                self.data = Modbus.ReadWriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_registers:
                self.data = Modbus.WriteMultipleRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_file_record:
                self.data = Modbus.WriteFileRecordRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_register:
                self.data = Modbus.WriteSingleRegisterRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_single_coil:
                self.data = Modbus.WriteSingleCoilRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_holding_registers:
                self.data = Modbus.ReadHoldingRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_device_identification:
                self.data = Modbus.ReadDeviceIdentificationRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.write_multiple_coils:
                self.data = Modbus.WriteMultipleCoilsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_file_record:
                self.data = Modbus.ReadFileRecordRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_coils:
                self.data = Modbus.ReadCoilsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_discrete_inputs:
                self.data = Modbus.ReadDiscreteInputsRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.read_input_registers:
                self.data = Modbus.ReadInputRegistersRequest(self._io, self, self._root)
            elif _on == Modbus.FunctionCodes.mask_write_register:
                self.data = Modbus.MaskWriteRegisterRequest(self._io, self, self._root)
            else:
                self.data = Modbus.RawRequest(self._io, self, self._root)


    class WriteSingleCoilRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.output_address = self._io.read_u2be()
            self.output_value = KaitaiStream.resolve_enum(Modbus.CoilStates, self._io.read_u2be())


    class RawRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.raw_data = []
            i = 0
            while not self._io.is_eof():
                self.raw_data.append(self._io.read_u1())
                i += 1



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


    class FileRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_type = KaitaiStream.resolve_enum(Modbus.ReferenceTypes, self._io.read_u1())
            self.file_number = self._io.read_u2be()
            self.record_number = self._io.read_u2be()
            self.record_length = self._io.read_u2be()


    class WriteFileRecordRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_count = self._io.read_u1()
            self.records = []
            for i in range((self.byte_count - 1) // 7):
                self.records.append(Modbus.WriteFileRecord(self._io, self, self._root))



    class ReadHoldingRegistersRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_registers = self._io.read_u2be()
            if not self.quantity_of_registers >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_registers, self._io, u"/types/read_holding_registers_request/seq/1")
            if not self.quantity_of_registers <= 125:
                raise kaitaistruct.ValidationGreaterThanError(125, self.quantity_of_registers, self._io, u"/types/read_holding_registers_request/seq/1")


    class WriteFileRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reference_type = KaitaiStream.resolve_enum(Modbus.ReferenceTypes, self._io.read_u1())
            self.file_number = self._io.read_u2be()
            self.record_number = self._io.read_u2be()
            self.record_length = self._io.read_u2be()
            self.record_data = []
            for i in range(self.record_length):
                self.record_data.append(self._io.read_u2be())



    class ReadCoilsRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.starting_address = self._io.read_u2be()
            self.quantity_of_coils = self._io.read_u2be()
            if not self.quantity_of_coils >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.quantity_of_coils, self._io, u"/types/read_coils_request/seq/1")
            if not self.quantity_of_coils <= 2000:
                raise kaitaistruct.ValidationGreaterThanError(2000, self.quantity_of_coils, self._io, u"/types/read_coils_request/seq/1")


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


    class ReadDeviceIdentificationRequest(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mei_type = KaitaiStream.resolve_enum(Modbus.MeiTypes, self._io.read_u1())
            self.object_id = self._io.read_u1()
            self.object_value = self._io.read_u1()



