module sqlite3-db-claude-3-5-sonnet-20241022

import stdlib::*

def Main = {
    header : Header
    pages : [Page]
}

def Header = {
    magic : Magic
    pageSize : uint16
    fileFormatWrite : uint8
    fileFormatRead : uint8
    unusedSpace : uint8
    maxEmbeddedPayload : uint8
    minEmbeddedPayload : uint8
    leafPayload : uint8
    fileChangeCounter : uint32
    databaseSize : uint32
    firstFreelistTrunk : uint32
    totalFreelistPages : uint32
    schemaCookie : uint32
    schemaFormat : uint32
    defaultPageCache : uint32
    largestRoot : uint32
    textEncoding : uint32
    userVersion : uint32
    vacuumMode : uint32
    appVersion : uint32
    reserved : uint32[20]
    validForVersion : uint32
    sqliteVersion : uint32
}

def Magic = {
    @assert $ == "SQLite format 3\x00"
    _ : uint8[16]
}

def Page = {
    @assert offset % pageSize == 0
    content : uint8[pageSize]
}

def PageType = [
    InteriorIndex = 2
    InteriorTable = 5
    LeafIndex = 10
    LeafTable = 13
]

def Cell = {
    cellType : PageType
    rowId : uint64
    data : [uint8]
}

let pageSize = 4096