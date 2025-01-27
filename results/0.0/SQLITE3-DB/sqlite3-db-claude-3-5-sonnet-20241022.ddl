module sqlite3-db-claude-3-5-sonnet-20241022

import Daedalus.Core
import Daedalus.Core.Basics

def Main = {
    header = SQLiteHeader
    body = Many0(SQLiteRecord)
}

def SQLiteHeader = {
    magic = UInt8[4] == [0x53, 0x51, 0x4C, 0x69]  -- "SQLi"
    version = UInt8[2] == [0x74, 0x65]            -- "te"
    format = UInt8 == 0x03                         -- Format 3
}

def SQLiteRecord = {
    recordType = UInt8
    Choose {
        recordType == 0x05 => TableRecord
        recordType == 0x0D => IndexRecord
        recordType == 0x11 => DataRecord
    }
}

def TableRecord = {
    length = VarInt
    name = UTF8String(length)
    columns = UInt16
    columnDefs = Array(columns, ColumnDef)
}

def IndexRecord = {
    length = VarInt
    name = UTF8String(length)
    tableRef = VarInt
    unique = UInt8
    columns = UInt16
    indexColumns = Array(columns, IndexColumn)
}

def DataRecord = {
    rowid = VarInt
    length = VarInt
    data = Bytes(length)
}

def ColumnDef = {
    nameLen = VarInt
    name = UTF8String(nameLen)
    type = UInt8
    constraints = UInt8
}

def IndexColumn = {
    columnRef = VarInt
    collation = UInt8
    sort = UInt8
}

def VarInt = {
    first = UInt8
    Choose {
        first < 0x80 => Value(first)
        Otherwise => {
            rest = UInt8[8]
            Value(DecodeVarInt(first : rest))
        }
    }
}

def DecodeVarInt(bytes) = {
    result = 0
    For i from 0 to Size(bytes) - 1 do {
        result = (result << 7) | (bytes[i] & 0x7F)
    }
    result
}

def UTF8String(length) = {
    chars = UInt8[length]
    Value(DecodeUTF8(chars))
}