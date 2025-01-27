def Main = {
  ModbusFrame
}

def ModbusFrame = {
  $$ = {
    address : uint 8;
    function_code : uint 8;
    data : FunctionData(function_code);
    crc : uint 16
  }
}

def FunctionData(code: uint 8) = {
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
    _ => UnknownFunctionData
  }
}

def ReadCoilsRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16
  }
}

def ReadDiscreteInputsRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16
  }
}

def ReadHoldingRegistersRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16
  }
}

def ReadInputRegistersRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16
  }
}

def WriteSingleCoilRequest = {
  $$ = {
    output_address : uint 16;
    output_value : uint 16
  }
}

def WriteSingleRegisterRequest = {
  $$ = {
    register_address : uint 16;
    register_value : uint 16
  }
}

def WriteMultipleCoilsRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16;
    byte_count : uint 8;
    coil_values : Array byte_count uint 8
  }
}

def WriteMultipleRegistersRequest = {
  $$ = {
    starting_address : uint 16;
    quantity : uint 16;
    byte_count : uint 8;
    register_values : Array (byte_count / 2) uint 16
  }
}

def ExceptionResponse = {
  $$ = {
    exception_code : uint 8
  }
}

def UnknownFunctionData = {
  $$ = null
}

def Array (n: uint) (type: Type) = {
  @Stream {
    value : type
  } repeat n
}