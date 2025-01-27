module Modbus where

import Daedalus

-- Define the Modbus PDU (Protocol Data Unit)
def ModbusPDU =
  block
    transactionID: UInt16
    protocolID: UInt16
    length: UInt16
    unitID: UInt8
    functionCode: UInt8
    data: Bytes

-- Define the Modbus ADU (Application Data Unit)
def ModbusADU =
  block
    header: ModbusPDU
    crc: UInt16

-- Define the Modbus frame
def ModbusFrame =
  block
    start: UInt8 = 0x3A
    adu: ModbusADU
    end: UInt8 = 0x0D

-- Define the main parser
def Main =
  block
    frame: ModbusFrame
    $$ = frame