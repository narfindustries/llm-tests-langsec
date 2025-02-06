def MBAP = record {
    trans_id: integer[width=16, endian=big];
    proto_id: integer[width=16, endian=big];
    length: integer[width=16, endian=big];
    unit_id: integer[width=8];
};

def ReadCoilsRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
};

def ReadCoilsResponse = record {
    byte_count: integer[width=8];
    coil_status: integer[width=8][byte_count];
};

def ReadDiscreteInputsRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
};

def ReadDiscreteInputsResponse = record {
    byte_count: integer[width=8];
    input_status: integer[width=8][byte_count];
};

def ReadHoldingRegistersRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
};

def ReadHoldingRegistersResponse = record {
    byte_count: integer[width=8];
    register_value: integer[width=8][byte_count];
};

def ReadInputRegistersRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
};

def ReadInputRegistersResponse = record {
    byte_count: integer[width=8];
    register_value: integer[width=8][byte_count];
};

def WriteSingleCoilRequest = record {
    output_addr: integer[width=16, endian=big];
    output_value: integer[width=16, endian=big];
};

def WriteSingleRegisterRequest = record {
    register_addr: integer[width=16, endian=big];
    register_value: integer[width=16, endian=big];
};

def WriteMultipleCoilsRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
    byte_count: integer[width=8];
    output_value: integer[width=8][byte_count];
};

def WriteMultipleRegistersRequest = record {
    start_addr: integer[width=16, endian=big];
    quantity: integer[width=16, endian=big];
    byte_count: integer[width=8];
    register_value: integer[width=8][byte_count];
};

def ReadFileRecordRequest = record {
    byte_count: integer[width=8];
    subreq_reference: integer[width=8];
    file_number: integer[width=16, endian=big];
    record_number: integer[width=16, endian=big];
    record_length: integer[width=16, endian=big];
};

def WriteFileRecordRequest = record {
    byte_count: integer[width=8];
    subreq_reference: integer[width=8];
    file_number: integer[width=16, endian=big];
    record_number: integer[width=16, endian=big];
    record_length: integer[width=16, endian=big];
    record_data: integer[width=8][record_length];
};

def MaskWriteRegisterRequest = record {
    reference_addr: integer[width=16, endian=big];
    and_mask: integer[width=16, endian=big];
    or_mask: integer[width=16, endian=big];
};

def ReadDeviceIdRequest = record {
    mei_type: integer[width=8];
    read_device_id_code: integer[width=8];
    object_id: integer[width=8];
};

def ErrorResponse = record {
    exception_code: integer[width=8];
};

def ModbusFunction = choice function_code {
    1 -> ReadCoilsRequest;
    2 -> ReadDiscreteInputsRequest;
    3 -> ReadHoldingRegistersRequest;
    4 -> ReadInputRegistersRequest;
    5 -> WriteSingleCoilRequest;
    6 -> WriteSingleRegisterRequest;
    15 -> WriteMultipleCoilsRequest;
    16 -> WriteMultipleRegistersRequest;
    20 -> ReadFileRecordRequest;
    21 -> WriteFileRecordRequest;
    22 -> MaskWriteRegisterRequest;
    43 -> ReadDeviceIdRequest;
    129..143 -> ErrorResponse;
};

def ModbusTCP = record {
    header: MBAP;
    function_code: integer[width=8];
    data: ModbusFunction;
};

def ModbusRTU = record {
    address: integer[width=8];
    function_code: integer[width=8];
    data: ModbusFunction;
    crc: integer[width=16, endian=little];
};