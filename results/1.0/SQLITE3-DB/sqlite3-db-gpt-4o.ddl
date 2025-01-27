format sqlite3-db-gpt-4o {
    // SQLite database file format version 3 specification

    const headerString = "SQLite format 3\0";
    const pageSizeOffset = 16;

    enum PageType : uint8 {
        Unknown = 0,
        InteriorIndexBTree = 2,
        InteriorTableBTree = 5,
        LeafIndexBTree = 10,
        LeafTableBTree = 13
    }

    struct DbHeader {
        magic: bytes[16] : assert(magic == headerString);
        pageSize: uint16le;
        writeVersion: uint8;
        readVersion: uint8;
        reservedSpace: uint8;
        maxPayloadFraction: uint8;
        minPayloadFraction: uint8;
        leafPayloadFraction: uint8;
        fileChangeCounter: uint32le;
        reservedBytes: uint32le[4];
    }

    struct BTreePage {
        pageType: PageType;
        firstFreeBlock: uint16le;
        cellCount: uint16le;
        cellContentArea: uint16le;
        fragmentedFreeBytes: uint8;
        rightMostPointer: uint32le : pageType in [PageType.InteriorIndexBTree, PageType.InteriorTableBTree];
        cells: CellArray(cellCount);
    }

    struct CellArray(count: uint16) {
        cells: Cell[count];
    }

    struct Cell {
        // Definition of cell based on cell types can be added here
        // Placeholder for different cell contents
        content: bytes[variableSize]; // For example purposes only
    }

    main {
        header: DbHeader;
        pages: BTreePage[calculatePageCount()];
    }

    // Helper function to calculate the number of pages
    fn calculatePageCount(): uint32 {
        return (file.size - pageSizeOffset) / header.pageSize;
    }
}