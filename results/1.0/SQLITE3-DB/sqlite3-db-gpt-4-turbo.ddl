module SQLITE3_DB {
    doc: "This module parses the SQLite database format";
    major_version: 1 minor_version: 0

    import std.io
    import std.codecs
    import std.ascii

    structure Database {
        value header: Header
        value btreePages: [BtreePage] = btreePages(header: header, offset: 100)

        function btreePages(header: Header, offset: uint64) -> [BtreePage] {
            if (stream.remaining() == 0) return []
            dbOffset: uint32 = stream.tell()
            page: BtreePage = BtreePage()
            nextOffset: uint32 = dbOffset + header.pageSize
            stream.seek(nextOffset)
            return [page] ++ btreePages(header, nextOffset)
        }
    }

    structure Header {
        const magicHeader: [uint8] = [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00]
        value pageSize: uint16
        value writeVersion: uint8
        value readVersion: uint8
        value reservedSpace: uint8
        value maxPayloadFraction: uint8
        value minPayloadFraction: uint8
        value leafPayloadFraction: uint8
        value fileChangeCounter: uint32
        value databaseSize: uint32
        value freelistFirstPage: uint32
        value freelistPageCount: uint32
        value schemaCookie: uint32
        value schemaFormat: uint32
        value pageCacheSize: uint32
        value pageVacuumNumber: uint32
        value textEncoding: uint32
        value userVersion: uint32
        value incrementalVacuumMode: uint32
        value applicationId: uint32
        padding reserved: [uint8] = [0; 20]
        value versionValidFor: uint32
        value sqliteVersion: uint32
    }

    structure BtreePage {
        value pageType: uint8
        value firstFreeblock: uint16
        value nCells: uint16
        value cellContentAreaOffset: uint16
        value nFragmentedFreeBytes: uint8
        value cells: [Cell] = array(nCells, Cell(reader))

        structure Cell {
            value leftChildPage: switch(pageType) {{1, 2, 5} -> uint32 }
            value payloadSize: switch(pageType) {{1, 5} -> varint }
            value rowId: switch(pageType) {{1, 5} -> varint_nil }
            value data: switch(pageType) {{1, 5} -> [uint8](payloadSize - (condition(stream.tell() < cellContentAreaOffset))) }
        }
    }
}