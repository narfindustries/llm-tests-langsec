modbus =
  type Address = uint8 with min: 0 and max: 247
  type FunctionCode = uint8 with min: 1 and max: 127
  type ErrorCode = uint8 with min: 1 and max: 127
  type ByteCount = uint8 with min: 1 and max: 255
  type ReferenceType = uint8 with min: 0 and max: 3
  type ReferenceNumber = uint16 with min: 0 and max: 65535
  type SubRequest = uint8 with min: 0 and max: 255
  type ReadWriteLength = uint16 with min: 0 and max: 65535

  type ModbusMessage =
    address: Address
    functionCode: FunctionCode
    data: Data

  type Data =
    ReadCoilStatus: ReadCoilStatus
    ReadInputStatus: ReadInputStatus
    ReadHoldingRegisters: ReadHoldingRegisters
    ReadInputRegisters: ReadInputRegisters
    ForceSingleCoil: ForceSingleCoil
    PresetSingleRegister: PresetSingleRegister
    ReadWriteMultipleCoils: ReadWriteMultipleCoils
    ReadWriteMultipleHoldingRegisters: ReadWriteMultipleHoldingRegisters
    ReadWriteMultipleHoldingRegistersExtended: ReadWriteMultipleHoldingRegistersExtended
    ReportSlaveID: ReportSlaveID
    ProgramController: ProgramController
    ProgramControllerMaskWrite: ProgramControllerMaskWrite
    ProgramControllerRead: ProgramControllerRead
    ForceMultipleCoils: ForceMultipleCoils
    PresetMultipleRegisters: PresetMultipleRegisters
    ReportSlaveIDExtended: ReportSlaveIDExtended
    ReadFileRecord: ReadFileRecord
    WriteFileRecord: WriteFileRecord
    MaskWriteRegister: MaskWriteRegister
    ReadWriteRegisters: ReadWriteRegisters
    ReadFileRecordExtended: ReadFileRecordExtended
    WriteFileRecordExtended: WriteFileRecordExtended
    ReadFileRecordSubrequest: ReadFileRecordSubrequest
    WriteFileRecordSubrequest: WriteFileRecordSubrequest

  type ReadCoilStatus =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 0
    referenceNumber: ReferenceNumber

  type ReadInputStatus =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 1
    referenceNumber: ReferenceNumber

  type ReadHoldingRegisters =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber

  type ReadInputRegisters =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 3
    referenceNumber: ReferenceNumber

  type ForceSingleCoil =
    referenceType: ReferenceType with value: 0
    referenceNumber: ReferenceNumber
    value: boolean

  type PresetSingleRegister =
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber
    value: uint16

  type ReadWriteMultipleCoils =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 0
    referenceNumber: ReferenceNumber

  type ReadWriteMultipleHoldingRegisters =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber

  type ReadWriteMultipleHoldingRegistersExtended =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber

  type ReportSlaveID =
    byteCount: ByteCount
    slaveID: Address

  type ProgramController =
    subRequest: SubRequest

  type ProgramControllerMaskWrite =
    subRequest: SubRequest

  type ProgramControllerRead =
    subRequest: SubRequest

  type ForceMultipleCoils =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 0
    referenceNumber: ReferenceNumber

  type PresetMultipleRegisters =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber

  type ReportSlaveIDExtended =
    byteCount: ByteCount
    slaveID: Address

  type ReadFileRecord =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type WriteFileRecord =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type MaskWriteRegister =
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber
    value: uint16

  type ReadWriteRegisters =
    byteCount: ByteCount
    referenceType: ReferenceType with value: 2
    referenceNumber: ReferenceNumber

  type ReadFileRecordExtended =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type WriteFileRecordExtended =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type ReadFileRecordSubrequest =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type WriteFileRecordSubrequest =
    subRequest: SubRequest
    referenceNumber: ReferenceNumber

  type ErrorResponse =
    address: Address
    functionCode: FunctionCode
    errorCode: ErrorCode

default ModbusMessage =
  address: 0
  functionCode: 1
  data: undefined