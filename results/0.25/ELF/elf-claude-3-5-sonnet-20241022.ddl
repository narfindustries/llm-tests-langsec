def ELF = {
    let ident = {
        magic0: uint8 | magic0 == 0x7f,
        magic1: uint8 | magic1 == 0x45,
        magic2: uint8 | magic2 == 0x4c,
        magic3: uint8 | magic3 == 0x46,
        class: uint8,
        data: uint8,
        version: uint8,
        osabi: uint8,
        abiversion: uint8,
        pad: uint8[7]
    };

    let header32 = {
        type: uint16,
        machine: uint16,
        version: uint32,
        entry: uint32,
        phoff: uint32,
        shoff: uint32,
        flags: uint32,
        ehsize: uint16,
        phentsize: uint16,
        phnum: uint16,
        shentsize: uint16,
        shnum: uint16,
        shstrndx: uint16
    };

    let header64 = {
        type: uint16,
        machine: uint16,
        version: uint32,
        entry: uint64,
        phoff: uint64,
        shoff: uint64,
        flags: uint32,
        ehsize: uint16,
        phentsize: uint16,
        phnum: uint16,
        shentsize: uint16,
        shnum: uint16,
        shstrndx: uint16
    };

    let phdr32 = {
        type: uint32,
        offset: uint32,
        vaddr: uint32,
        paddr: uint32,
        filesz: uint32,
        memsz: uint32,
        flags: uint32,
        align: uint32
    };

    let phdr64 = {
        type: uint32,
        flags: uint32,
        offset: uint64,
        vaddr: uint64,
        paddr: uint64,
        filesz: uint64,
        memsz: uint64,
        align: uint64
    };

    let shdr32 = {
        name: uint32,
        type: uint32,
        flags: uint32,
        addr: uint32,
        offset: uint32,
        size: uint32,
        link: uint32,
        info: uint32,
        addralign: uint32,
        entsize: uint32
    };

    let shdr64 = {
        name: uint32,
        type: uint32,
        flags: uint64,
        addr: uint64,
        offset: uint64,
        size: uint64,
        link: uint32,
        info: uint32,
        addralign: uint64,
        entsize: uint64
    };

    let sym32 = {
        name: uint32,
        value: uint32,
        size: uint32,
        info: uint8,
        other: uint8,
        shndx: uint16
    };

    let sym64 = {
        name: uint32,
        info: uint8,
        other: uint8,
        shndx: uint16,
        value: uint64,
        size: uint64
    };

    let rel32 = {
        offset: uint32,
        info: uint32
    };

    let rela32 = {
        offset: uint32,
        info: uint32,
        addend: int32
    };

    let rel64 = {
        offset: uint64,
        info: uint64
    };

    let rela64 = {
        offset: uint64,
        info: uint64,
        addend: int64
    };

    let dyn32 = {
        tag: int32,
        val: uint32
    };

    let dyn64 = {
        tag: int64,
        val: uint64
    };

    ident;
    if (ident.class == 1) {
        header32;
        phdr32[header32.phnum];
        shdr32[header32.shnum];
    } else if (ident.class == 2) {
        header64;
        phdr64[header64.phnum];
        shdr64[header64.shnum];
    }
}