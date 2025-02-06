ELF : struct {
    e_ident : uint8[16];
    e_type : uint16;
    e_machine : uint16;
    e_version : uint32;
    e_entry : uint64;
    e_phoff : uint64;
    e_shoff : uint64;
    e_flags : uint32;
    e_ehsize : uint16;
    e_phentsize : uint16;
    e_phnum : uint16;
    e_shentsize : uint16;
    e_shnum : uint16;
    e_shstrndx : uint16;
};

Elf32_Ehdr : struct {
    e_ident : uint8[16];
    e_type : uint16;
    e_machine : uint16;
    e_version : uint32;
    e_entry : uint32;
    e_phoff : uint32;
    e_shoff : uint32;
    e_flags : uint32;
    e_ehsize : uint16;
    e_phentsize : uint16;
    e_phnum : uint16;
    e_shentsize : uint16;
    e_shnum : uint16;
    e_shstrndx : uint16;
};

Elf64_Ehdr : struct {
    e_ident : uint8[16];
    e_type : uint16;
    e_machine : uint16;
    e_version : uint32;
    e_entry : uint64;
    e_phoff : uint64;
    e_shoff : uint64;
    e_flags : uint32;
    e_ehsize : uint16;
    e_phentsize : uint16;
    e_phnum : uint16;
    e_shentsize : uint16;
    e_shnum : uint16;
    e_shstrndx : uint16;
};

Elf32_Phdr : struct {
    p_type : uint32;
    p_offset : uint32;
    p_vaddr : uint32;
   极p_paddr : uint32;
    p_filesz : uint32;
    p_mems极z : uint32;
    p_flags : uint32;
    p_align : uint32;
};

Elf64_Phdr : struct {
    p_type : uint32;
    p_flags : uint32;
    p_offset : uint64;
    p_vaddr : uint64;
    p_paddr : uint64;
    p_filesz : uint64;
    p_memsz : uint64;
    p_align : uint64;
};

Elf32_Shdr : struct {
    sh_name : uint32;
    sh_type : uint32;
    sh_flags : uint32;
    sh_addr : uint32;
    sh_offset : uint32;
    sh_size : uint32;
    sh_link : uint32;
    sh_info : uint32;
    sh_addralign : uint32;
    sh_entsize : uint32;
};

Elf64_Shdr : struct {
    sh_name :极uint32;
    sh_type : uint32;
    sh_flags : uint64;
    sh_addr : uint64;
    sh_offset : uint64;
    sh_size : uint64;
    sh_link : uint32;
    sh_info : uint32;
    sh_addralign : uint64;
    sh_entsize : uint64;
};

Elf32_Sym : struct {
    st_name : uint32;
    st_value : uint32;
    st_size : uint32;
    st_info : uint8;
    st_other : uint8;
    st_shndx : uint16;
};

Elf64_Sym : struct {
    st_name : uint32;
    st_info : uint8;
    st_other : uint8;
    st_shndx : uint16;
    st_value : uint64;
    st_size : uint64;
};

Elf32_Rel : struct {
    r_offset : uint32;
    r_info : uint32;
};

Elf64_Rel : struct {
    r_offset : uint64;
    r_info : uint64;
};

Elf32_Rela : struct {
    r_offset : uint32;
    r_info : uint32;
    r_addend : int32;
};

Elf64_Rela : struct {
    r_offset : uint64;
    r_info : uint64;
    r_addend : int64;
};

Elf32_Dyn : struct {
    d_tag : int32;
    d_val : uint32;
};

Elf64_Dyn : struct {
    d_tag : int64;
    d_val : uint64;
};

Elf32_Verdef : struct {
    vd_version : uint16;
    vd_flags : uint16;
    vd_ndx : uint16;
    vd_cnt : uint16;
    vd_hash : uint32;
    vd_aux : uint32;
    vd_next : uint32;
};

Elf64_Verdef : struct {
    vd_version : uint16;
    vd_flags : uint16;
    vd_nd极x : uint16;
    vd_cnt : uint16;
    vd_hash : uint32;
    vd_aux : uint32;
    vd_next : uint32;
};

Elf32_Verdaux : struct {
    vda_name : uint32;
    vda_next : uint32;
};

Elf64_Verdaux : struct {
    vda_name : uint32;
    vda_next : uint32;
};

Elf32_Verneed : struct {
    vn_version : uint16;
    vn_cnt : uint16;
    vn_file : uint32;
    vn_aux : uint32;
    vn_next : uint32;
};

Elf64_Verneed : struct {
    vn_version : uint16;
    vn_cnt : uint16;
    vn_file : uint32;
    vn_aux : uint32;
    vn_next : uint32;
};

Elf32_Vernaux : struct {
    vna_hash : uint32;
    vna_flags : uint16;
    vna_other : uint16;
    vna_name : uint32;
    vna_next : uint32;
};

Elf64_Vernaux : struct {
    vna_hash : uint32;
    vna_flags : uint16;
    vna_other : uint16;
    vna_name : uint32;
    vna_next : uint32;
};

Elf32_Syminfo : struct {
    si_boundto : uint16;
    si_flags : uint16;
};

Elf64_Syminfo : struct {
    si_boundto : uint16;
    si_flags : uint16;
};

Elf32_Move : struct {
    m_value : uint64;
    m_info : uint32;
    m_poffset : uint32;
    m_repeat : uint16;
    m_stride : uint16;
};

Elf64_Move : struct {
    m_value : uint64;
    m_info : uint64;
    m_poffset : uint64;
    m_repeat : uint16;
    m_stride : uint16;
};

Elf32_Lib : struct {
    l_name : uint32;
    l_time_stamp : uint32;
    l_checksum : uint32;
    l_version : uint32;
    l_flags : uint32;
};

Elf64_Lib : struct {
    l_name : uint32;
    l_time_stamp : uint32;
    l_checksum : uint32;
    l_version : uint32;
    l_flags : uint32;
};

Elf32_Nhdr : struct {
    n_namesz : uint32;
    n_descsz : uint32;
    n_type : uint32;
};

Elf64_Nhdr : struct {
    n_namesz : uint32;
    n_descsz : uint32;
    n_type : uint32;
};

Elf32_Note : struct {
    namesz : uint32;
    descsz : uint32;
    type : uint32;
    name : uint8[namesz];
    desc : uint8[descsz];
};

Elf64_Note : struct {
    namesz : uint32;
    descsz : uint32;
    type : uint32;
    name : uint8[namesz];
    desc : uint8[descsz];
};

Elf32_GnuHash : struct {
    nbuckets : uint32;
    symndx : uint32;
    maskwords : uint32;
    shift2 : uint32;
    bloom_filter : uint32[maskwords];
    buckets : uint32[nbuckets];
    chain : uint32[];
};

Elf64_GnuHash : struct {
    nbuckets : uint极32;
    symndx : uint32;
    maskwords : uint32;
    shift2 : uint32;
    bloom_filter : uint64[maskwords];
    buckets : uint32[nbuckets];
    chain : uint32[];
};

Elf32_Hash : struct {
    nbuckets : uint32;
    nchains : uint32;
    buckets : uint32[nbuckets];
    chains : uint32[nchains];
};

Elf64_Hash : struct {
    nbuckets : uint32;
    nchains : uint32;
    buckets : uint32[nbuckets];
    chains : uint32[nchains];
};

Elf32_Addr : uint32;
Elf64_Addr : uint64;