def Main = {
    Magic;
    FileHeader;
    SectionHeaders;
    SectionData
}

def Magic = {
    $7f @ 0;
    'E' @ 1;
    'L' @ 2;
    'F' @ 3
}

def FileHeader = {
    class : uint8;
    data : uint8;
    version : uint8;
    osabi : uint8;
    abiversion : uint8;
    pad : uint8[7];
    type : uint16;
    machine : uint16;
    version2 : uint32;
    entry : uint32;
    phoff : uint32;
    shoff : uint32;
    flags : uint32;
    ehsize : uint16;
    phentsize : uint16;
    phnum : uint16;
    shentsize : uint16;
    shnum : uint16;
    shstrndx : uint16
}

def SectionHeader = {
    name : uint32;
    type : uint32;
    flags : uint32;
    addr : uint32;
    offset : uint32;
    size : uint32;
    link : uint32;
    info : uint32;
    addralign : uint32;
    entsize : uint32
}

def SectionHeaders = {
    headers : SectionHeader[^FileHeader.shnum]
}

def SectionData = {
    sections : forall i < ^FileHeader.shnum {
        SetPosition (^FileHeader.shoff + i * ^FileHeader.shentsize);
        data : uint8[^SectionHeaders.headers[i].size] @ ^SectionHeaders.headers[i].offset
    }
}