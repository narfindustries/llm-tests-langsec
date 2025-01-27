module HL7.Messages.V2.GPT4Turbo {
    using HL7.Types;
    using HL7.Utils;

    def parse = many1 (segment);

    def segment = 
        msh_seg <|> 
        pid_seg <|> 
        orc_seg <|>
        obr_seg <|> 
        obx_seg <|>
        custom_logic;

    def msh_seg = 
        Segment ('MSH') {
            [ fieldSep    : pos 1 : char,
              encChars    : pos 2 : EncodingChars,
              sendingApp  : pos 3 : Text,
              sendingFac  : pos 4 : Text,
              recvApp     : pos 5 : Text,
              recvFac     : pos 6 : Text,
              date        : pos 7 : HL7Date,
              security    : pos 8 : nullable Text,
              msgType     : pos 9 : msg_type,
              msgControl  : pos 10: Text,
              procId      : pos 11: Text,
              versionId   : pos 12: Text
            ] @ delim fieldSep
        }

    def pid_seg = 
        Segment ('PID') {
            [ patientId      : pos 3 : nullable Text,
              patientName    : pos 5 : PersonName,
              motherMaiden   : pos 6 : nullable Text,
              patientAddr    : pos 11: nullable Text,
              countyCode     : pos 12: nullable Text
            ] @ delim fieldSep
        }

    def orc_seg = 
        Segment ('ORC') {
            [ orderControl   : pos 1 : Text,
              plcOrderNo     : pos 2 : nullable Text,
              fillerOrderNo  : pos 3 : nullable Text,
              placerGroupNo  : pos 4 : nullable Text,
              orderStatus    : pos 5 : nullable Text,
              responseFlag   : pos 6 : nullable Text
            ] @ delim fieldSep
        }

    def obr_seg = 
        Segment ('OBR') {
            [ setID          : pos 1 : nullable Text,
              placerOrderNo  : pos 2 : Text,
              fillerOrderNo  : pos 3 : Text,
              universalSvcID : pos 4 : Text,
              priority       : pos 5 : nullable Text
            ] @ delim fieldSep
        }

    def obx_seg = 
        Segment ('OBX') {
            [ setID      : pos 1 : nullable Text,
              valueType  : pos 2 : Text,
              obsID      : pos 3 : Text,
              obsSubID   : pos 4 : nullable Text,
              obsValue   : pos 5 : Text,
              units      : pos 6 : Text,
              refRange   : pos 7 : nullable Text,
              abnormal   : pos 8 : nullable Text,
              likelihood : pos 9 : nullable Text
            ] @ delim fieldSep
        }
        
    -- Define Encoding Characters structure
    def EncodingChars = struct {
        componentSep    : char,
        repeatSep       : char,
        escapeChar      : char,
        subcomponentSep : char
    }
    
    -- Define Message Type structure
    def msg_type = struct {
        messageType : Text,
        trigEvent   : Text
    } @ delim '^'

    -- Define Person Name structure
    def PersonName = struct {
        familyName : nullable Text,
        givenName  : nullable Text,
        middleInit : nullable Text,
        suffix     : nullable Text,
        prefix     : nullable Text
    } @ delim '^'
    
    -- Define optional logic for custom segment if present
    def custom_logic = Segment (custom) {
        generic_fields : Bytes
    } @ catch return none; -- handle unknown segments gracefully
}