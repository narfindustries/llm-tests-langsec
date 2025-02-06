module DICOM;

import std.uint8;
import std.uint16;
import std.uint32;
import std.int16;
import std.int32;
import std.string;
import std.array;

// Define VR (Value Representation) types
alias VR_AE = string;
alias VR_AS = string;
alias VR_AT = uint16;
alias VR_CS = string;
alias VR_DA = string;
alias VR_DS = string;
alias VR_DT = string;
alias VR_IS = string;
alias VR_LO = string;
alias VR_LT = string;
alias VR_OB = array[uint8];
alias VR_OD = array[uint64];
alias VR_OF = array[uint32];
alias VR_OW = array[uint16];
alias VR_PN = string;
alias VR_SH = string;
alias VR_SL = int32;
alias VR_SQ = array[Element]; // Recursive definition for sequences
alias VR_SS = int16;
alias VR_ST = string;
alias VR_TM = string;
alias VR_UI = string;
alias VR_UL = uint32;
alias VR_UN = array[uint8];
alias VR_US = uint16;
alias VR_UT = string;

// Define a generic DICOM element
type Element = struct {
  tag: uint32;
  vr: string[2];
  length: uint32;
  value: switch (vr) {
    case "AE": VR_AE;
    case "AS": VR_AS;
    case "AT": VR_AT;
    case "CS": VR_CS;
    case "DA": VR_DA;
    case "DS": VR_DS;
    case "DT": VR_DT;
    case "IS": VR_IS;
    case "LO": VR_LO;
    case "LT": VR_LT;
    case "OB": VR_OB;
    case "OD": VR_OD;
    case "OF": VR_OF;
    case "OW": VR_OW;
    case "PN": VR_PN;
    case "SH": VR_SH;
    case "SL": VR_SL;
    case "SQ": VR_SQ;
    case "SS": VR_SS;
    case "ST": VR_ST;
    case "TM": VR_TM;
    case "UI": VR_UI;
    case "UL": VR_UL;
    case "UN": VR_UN;
    case "US": VR_US;
    case "UT": VR_UT;
    default: array[uint8];
  };
};

// Define a DICOM file structure
type DICOMFile = struct {
  elements: array[Element];
};