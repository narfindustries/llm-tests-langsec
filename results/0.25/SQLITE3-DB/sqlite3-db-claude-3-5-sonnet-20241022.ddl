module sqlite3-db-claude-3-5-sonnet-20241022

import stdlib::*

type SQLite3File = {
    header : Header
    body   : Body
}

type Header = {
    magic     : Magic
    pageSize  : uint16
    writeVer  : uint8
    readVer   : uint8
    spaceRes  : uint8
    maxFrac   : uint8
    minFrac   : uint8
    leafFrac  : uint8
    fileChg   : uint32
    dbSize    : uint32
    freePage  : uint32
    totalFree : uint32
    schemaCookie : uint32
    schemaFmt : uint32
    defPageCache : uint32
    largestRoot : uint32
    textEnc   : uint32
    userVer   : uint32
    incVacuum : uint32
    appId     : uint32
    reserved  : Array uint8 20
    verValid  : uint32
    version   : uint32
}

type Magic = {
    str : Array uint8 16
}

type Body = {
    pages : Array Page *
}

type Page = {
    content : Array uint8 *
}

let Magic_Value = [ 0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00 ]

let ValidPageSize = Choose [
    512,
    1024,
    2048,
    4096,
    8192,
    16384,
    32768,
    65536
]

let parse_sqlite3 = {
    file : SQLite3File
    file.header.magic.str == Magic_Value
    file.header.pageSize in ValidPageSize
    file.header.writeVer in 1..2
    file.header.readVer in 1..2
    file.header.spaceRes in 0..1
    file.header.maxFrac in 0..255
    file.header.minFrac in 0..255
    file.header.leafFrac in 0..255
    file.header.textEnc in [1, 2, 3]
}