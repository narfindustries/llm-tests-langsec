grammar NTPv4;

type Flags = packed struct {
    uint2 leapIndicator;
    uint3 versionNumber;
    uint3 mode;
};

type RootDelay = packed struct {
    uint16 seconds;
    uint16 fraction;
};

type RootDispersion = packed struct {
    uint16 seconds;
    uint16 fraction;
};

type Timestamp = packed struct {
    uint32 seconds;
    uint32 fraction;
};

type NTPHeader = packed struct {
    Flags flags;
    uint8 stratum;
    int8 poll;
    int8 precision;
    RootDelay rootDelay;
    RootDispersion rootDispersion;
    uint32 referenceIdentifier;
    Timestamp referenceTimestamp;
    Timestamp originTimestamp;
    Timestamp receiveTimestamp;
    Timestamp transmitTimestamp;
};

type ExtensionField = packed struct {
    uint16 fieldType;
    uint16 length;
    bytes value[length-4];
};

type NTPPacket = struct {
    NTPHeader header;
    ExtensionField[] extensions;
    optional uint32 keyIdentifier;
    optional bytes messageDigest[16];
};