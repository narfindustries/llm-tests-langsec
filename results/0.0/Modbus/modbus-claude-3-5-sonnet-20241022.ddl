def Main = {
  ModbusFrame
}

def ModbusFrame = {
  $$ = {
    transactionId: uint16BE;
    protocolId: uint16BE;
    length: uint16BE;
    unitId: uint8;
    functionCode: uint8;
    data: FunctionData(functionCode);
  }
}

def FunctionData (code: uint8) = {
  case code of {
    0x01 => ReadCoilsRequest;
    0x02 => ReadDiscreteInputsRequest;
    0x03 => ReadHoldingRegistersRequest;
    0x04 => ReadInputRegistersRequest;
    0x05 => WriteSingleCoilRequest;
    0x06 => WriteSingleRegisterRequest;
    0x0F => WriteMultipleCoilsRequest;
    0x10 => WriteMultipleRegistersRequest;
    0x81 => ExceptionResponse;
    0x82 => ExceptionResponse;
    0x83 => ExceptionResponse;
    0x84 => ExceptionResponse;
    0x85 => ExceptionResponse;
    0x86 => ExceptionResponse;
    0x8F => ExceptionResponse;
    0x90 => ExceptionResponse;
    _    => UnknownFunctionData
  }
}

def ReadCoilsRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfCoils: uint16BE;
  }
}

def ReadDiscreteInputsRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfInputs: uint16BE;
  }
}

def ReadHoldingRegistersRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfRegisters: uint16BE;
  }
}

def ReadInputRegistersRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfRegisters: uint16BE;
  }
}

def WriteSingleCoilRequest = {
  $$ = {
    outputAddress: uint16BE;
    outputValue: uint16BE;
  }
}

def WriteSingleRegisterRequest = {
  $$ = {
    registerAddress: uint16BE;
    registerValue: uint16BE;
  }
}

def WriteMultipleCoilsRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfOutputs: uint16BE;
    byteCount: uint8;
    outputValues: Array uint8 byteCount;
  }
}

def WriteMultipleRegistersRequest = {
  $$ = {
    startingAddress: uint16BE;
    quantityOfRegisters: uint16BE;
    byteCount: uint8;
    registerValues: Array uint16BE (byteCount / 2);
  }
}

def ExceptionResponse = {
  $$ = {
    exceptionCode: uint8;
  }
}

def UnknownFunctionData = {
  $$ = null
}