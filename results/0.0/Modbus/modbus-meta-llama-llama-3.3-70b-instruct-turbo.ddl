domain Modbus {
  type uint8  = uint<8>;
  type uint16 = uint<16>;
  type int16  = int<16>;

  type ModbusMessage {
    transactionId: uint16;
    protocolId: uint16;
    length: uint16;
    unitId: uint8;
    functionCode: uint8;
    data: Data;
  }

  type Data {
    union {
      coils: CoilStatus;
      registers: RegisterValues;
      exception: Exception;
    }
  }

  type CoilStatus {
    byteCount: uint8;
    coilBytes: array<uint8>;
  }

  type RegisterValues {
    byteCount: uint8;
    registerBytes: array<uint8>;
  }

  type Exception {
    exceptionCode: uint8;
  }

  grammar ModbusMessage {
    transactionId: uint16;
    protocolId: uint16;
    length: uint16;
    unitId: uint8;
    functionCode: uint8;
    data: Data;
  }

  grammar Data {
    coils: CoilStatus;
    registers: RegisterValues;
    exception: Exception;
  }

  grammar CoilStatus {
    byteCount: uint8;
    coilBytes: array<uint8>;
  }

  grammar RegisterValues {
    byteCount: uint8;
    registerBytes: array<uint8>;
  }

  grammar Exception {
    exceptionCode: uint8;
  }
}