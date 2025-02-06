module ELF where

import Daedalus.Type.AST
import Daedalus.Value

data ELFIdent = ELFIdent {
  ei_mag :: Array 4 Word8,
  ei_class :: Word8,
  ei_data :: Word8,
  ei_version :: Word8,
  ei_osabi :: Word8,
  ei_abiversion :: Word8,
  ei_pad :: Array 7 Word8
}

data ELFHeader = ELFHeader {
  e_ident :: ELFIdent,
  e_type :: Word16,
  e_machine :: Word16,
  e_version :: Word32,
  e_entry :: Word64,
  e_phoff :: Word64,
  e_shoff :: Word64,
  e_flags :: Word32,
  e_ehsize :: Word16,
  e_phentsize :: Word16,
  e_phnum :: Word16,
  e_shentsize :: Word16,
  e_shnum :: Word16,
  e_shstrndx :: Word16
}

data ProgramHeader = ProgramHeader {
  p_type :: Word32,
  p_flags :: Word32,
  p_offset :: Word64,
  p_vaddr :: Word64,
  p_paddr :: Word64,
  p_filesz :: Word64,
  p_memsz :: Word64,
  p_align :: Word64
}

data SectionHeader = SectionHeader {
  sh_name :: Word32,
  sh_type :: Word32,
  sh_flags :: Word64,
  sh_addr :: Word64,
  sh_offset :: Word64,
  sh_size :: Word64,
  sh_link :: Word32,
  sh_info :: Word32,
  sh_addralign :: Word64,
  sh_entsize :: Word64
}

data ELF = ELF {
  header :: ELFHeader,
  programHeaders :: Array (Maybe ProgramHeader),
  sectionHeaders :: Array (Maybe SectionHeader),
  sectionStrings :: ByteString, -- String table for section names
  sectionData :: Map Word32 ByteString -- Section index to section data
}


-- Note: This is still a simplified representation.  A complete Daedalus
-- specification would be vastly more complex, needing to handle all ELF
-- variants, optional fields, and potential variations in structure based
-- on architecture and ELF version.  Error handling and more robust type
-- checking would also be necessary.  This example provides a more
-- structurally correct starting point but remains incomplete for a full ELF
-- representation.  The section data is represented using a Map for easier
-- access by section index.  The section string table is included for
-- resolving section names.  Handling of different section types (e.g.,
-- symbol tables, relocation tables) would require further nested data
-- structures.
