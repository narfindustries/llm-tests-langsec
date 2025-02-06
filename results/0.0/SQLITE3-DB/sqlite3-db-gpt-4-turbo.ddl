module SQLite3;

import std::array;
import std::u8;
import std::u16;
import std::u32;
import std::u64;
import std::i64;
import std::bytes;

struct Header {
    magic: array[u8, 16],
    pageSize: u16,
    writeVersion: u8,
    readVersion: u8,
    reservedSpace: u8,
    maxPayloadFrac: u8,
    minPayloadFrac: u8,
    leafPayloadFrac: u8,
    fileChangeCounter: u32,
    databaseSize: u32,
    firstFreelistTrunkPage: u32,
    totalFreelistPages: u32,
    schemaCookie: u32,
    schemaFormat: u32,
    defaultCacheSize: u32,
    largestRootPage: u32,
    textEncoding: u32,
    userVersion: u32,
    incrementalVacuumMode: u32,
    applicationId: u32,
    reserved: array[u8, 20],
    versionValidFor: u32,
    sqliteVersion: u32
};

struct CellPointer {
    pointer: u16
};

struct BTreePage {
    pageType: u8,
    firstFreeblock: u16,
    numCells: u16,
    cellContentOffset: u16,
    numFragFreeBytes: u8,
    cellPointers: array[CellPointer],
    cells: array[Cell] @offset(cellContentOffset)
};

struct Cell {
    leftChildPage: u32,
    payloadSize: u32,
    rowId: i64,
    payload: Payload
};

struct Payload {
    data: bytes @length(payloadSize)
};

struct OverflowPage {
    nextPage: u32,
    content: bytes
};

struct FreelistPage {
    nextPage: u32,
    cells: bytes
};

struct PointerMapPage {
    entries: array[PointerMapEntry]
};

struct PointerMapEntry {
    type: u8,
    parentPageNumber: u32
};

struct DatabaseFile {
    header: Header,
    pages: array[Page]
};

union Page {
    case 0x02: InteriorIndexBTreePage,
    case 0x05: InteriorTableBTreePage,
    case 0x0A: LeafIndexBTreePage,
    case 0x0D: LeafTableBTreePage,
    case 0x0F: FreelistPage,
    case 0x10: OverflowPage,
    case 0x11: PointerMapPage
};

struct InteriorIndexBTreePage extends BTreePage;
struct InteriorTableBTreePage extends BTreePage;
struct LeafIndexBTreePage extends BTreePage;
struct LeafTableBTreePage extends BTreePage;

struct SQLiteDB {
    file: DatabaseFile
};