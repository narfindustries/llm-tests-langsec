unit nitf_meta_llama_llama_3_3 {
  length: 70;
  byte_order: big;

  field des_version: uint8 {
    doc: "Data Extension Header Version";
  }

  field des_type: uint8 {
    doc: "Data Extension Header Type";
  }

  field des_control: uint16 {
    doc: "Data Extension Header Control";
  }

  field userData: bytes {
    length: 64;
    doc: "User Defined Data Extension Header Data";
  }
}