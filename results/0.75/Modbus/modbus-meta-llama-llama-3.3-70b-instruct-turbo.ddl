type uint8 = byte
type uint16 = bytes(2)
type uint8_array = array(uint8)
type uint16_array = array(uint16)

define ModbusMessage {
  address: uint8(0..127),
  functionCode: uint8(
    ReadCoilStatus = 1,
    ReadInputStatus = 2,
    ReadHoldingRegisters = 3,
    ReadInputRegisters = 4,
    ForceSingleCoil = 5,
    PresetSingleRegister = 6,
    ReadExceptionStatus = 7,
    ForceMultipleCoils = 15,
    PresetMultipleRegisters = 16,
    ReportSlaveID = 17,
    ProgramController = 18,
    Program884 = 19,
    Program885 = 20,
    Program886 = 21,
    Program887 = 22,
    Program888 = 23
  ),
  data: choice(
    ReadCoilStatus: {
      startingAddress: uint16,
      quantityOfCoils: uint16
    },
    ReadInputStatus: {
      startingAddress: uint16,
      quantityOfInputs: uint16
    },
    ReadHoldingRegisters: {
      startingAddress: uint16,
      quantityOfRegisters: uint16
    },
    ReadInputRegisters: {
      startingAddress: uint16,
      quantityOfRegisters: uint16
    },
    ForceSingleCoil: {
      outputAddress: uint16,
      outputValue: uint8
    },
    PresetSingleRegister: {
      registerAddress: uint16,
      registerValue: uint16
    },
    ReadExceptionStatus: {},
    ForceMultipleCoils: {
      startingAddress: uint16,
      quantityOfCoils: uint16,
      outputValues: uint8_array
    },
    PresetMultipleRegisters: {
      startingAddress: uint16,
      quantityOfRegisters: uint16,
      registerValues: uint16_array
    },
    ReportSlaveID: {
      slaveID: uint8,
      runIndicatorStatus: uint8
    },
    ProgramController: {},
    Program884: {},
    Program885: {},
    Program886: {},
    Program887: {},
    Program888: {}
  )
}

define ModbusCRC {
  crc: uint16
}

root ModbusMessage