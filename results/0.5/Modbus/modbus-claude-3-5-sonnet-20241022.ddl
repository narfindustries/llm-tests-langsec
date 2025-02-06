type ModbusHeader = {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8
}

type FunctionCode = enum u8 {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06,
    WRITE_MULTIPLE_COILS = 0x0F,
    WRITE_MULTIPLE_REGISTERS = 0x10,
    READ_FILE_RECORD = 0x14,
    WRITE_FILE_RECORD = 0x15,
    MASK_WRITE_REGISTER = 0x16,
    READ_WRITE_MULTIPLE_REGISTERS = 0x17,
    READ_DEVICE_IDENTIFICATION = 0x2B
}

type ExceptionCode = enum u8 {
    ILLEGAL_FUNCTION = 0x01,
    ILLEGAL_DATA_ADDRESS = 0x02,
    ILLEGAL_DATA_VALUE = 0x03,
    SERVER_DEVICE_FAILURE = 0x04,
    ACKNOWLEDGE = 0x05,
    SERVER_DEVICE_BUSY = 0x06,
    MEMORY_PARITY_ERROR = 0x08,
    GATEWAY_PATH_UNAVAILABLE = 0x0A,
    GATEWAY_TARGET_FAILED = 0x0B
}

type ReadRequest = {
    starting_address: u16,
    quantity: u16
}

type ReadResponse = {
    byte_count: u8,
    data: bytes(byte_count)
}

type SingleWriteRequest = {
    output_address: u16,
    output_value: u16
}

type MultipleWriteRequest = {
    starting_address: u16,
    quantity: u16,
    byte_count: u8,
    data: bytes(byte_count)
}

type MultipleWriteResponse = {
    starting_address: u16,
    quantity: u16
}

type DeviceIdCode = enum u8 {
    BASIC = 0x01,
    REGULAR = 0x02,
    EXTENDED = 0x03,
    INDIVIDUAL = 0x04
}

type ObjectId = enum u8 {
    VENDOR_NAME = 0x00,
    PRODUCT_CODE = 0x01,
    MAJOR_MINOR_REVISION = 0x02
}

type DeviceIdentificationObject = {
    object_id: u8,
    object_length: u8,
    object_value: bytes(object_length)
}

type DeviceIdentificationRequest = {
    mei_type: u8,
    read_device_id_code: DeviceIdCode,
    object_id: ObjectId
}

type DeviceIdentificationResponse = {
    mei_type: u8,
    read_device_id_code: DeviceIdCode,
    conformity_level: u8,
    more_follows: u8,
    next_object_id: u8,
    number_of_objects: u8,
    objects: DeviceIdentificationObject[number_of_objects]
}

type ExceptionResponse = {
    function_code: u8,
    exception_code: ExceptionCode
}

type ModbusRequest = {
    header: ModbusHeader,
    function_code: FunctionCode,
    data: switch(function_code) {
        case READ_COILS, READ_DISCRETE_INPUTS,
             READ_HOLDING_REGISTERS, READ_INPUT_REGISTERS => ReadRequest,
        case WRITE_SINGLE_COIL, WRITE_SINGLE_REGISTER => SingleWriteRequest,
        case WRITE_MULTIPLE_COILS, WRITE_MULTIPLE_REGISTERS => MultipleWriteRequest,
        case READ_DEVICE_IDENTIFICATION => DeviceIdentificationRequest
    }
}

type ModbusResponse = {
    header: ModbusHeader,
    function_code: u8,
    data: switch(function_code) {
        case 0x01..0x04 => ReadResponse,
        case 0x05..0x06 => SingleWriteRequest,
        case 0x0F..0x10 => MultipleWriteResponse,
        case 0x2B => DeviceIdentificationResponse,
        case 0x81..0x8B => ExceptionResponse
    }
}

type ModbusMessage = {
    message: select {
        ModbusRequest,
        ModbusResponse
    }
}