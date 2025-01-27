specification DICOM {
    Version = 0.75

    def Main = {
        DicomFile *
    }

    def DicomFile = {
        @preamble? FileMetaInfo DataSet
    }

    def preamble = {
        $byte{128} "DICM"
    }

    def FileMetaInfo = {
        Group2Element+
    }

    def Group2Element = {
        @tag=(0x0002, $uint16) @length=$uint32 Value^length
    }

    def DataSet = {
        DataElement*
    }

    def DataElement = {
        @tag=($uint16, $uint16) @vr=VR @length=$uint16 Value^length
    }

    def Value = {
        $byte*
    }

    def VR = {
          "AE" | "AS" | "AT" | "CS" | "DA" | "DS" | "DT" | "FL" | "FD" | "IS"
        | "LO" | "LT" | "OB" | "OF" | "OW" | "PN" | "SH" | "SL" | "SQ" | "SS"
        | "ST" | "TM" | "UI" | "UL" | "UN" | "US" | "UT"
    }
}