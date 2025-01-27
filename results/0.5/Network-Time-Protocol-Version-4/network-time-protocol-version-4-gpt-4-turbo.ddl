module Network.Time.Protocol.Version4 {
  type NTPHeader = struct {
    li        : UInt2;   // Leap Indicator
    vn        : UInt3;   // Version Number
    mode      : UInt3;   // Mode
    stratum   : UInt8;   // Stratum
    poll      : Int8;    // Poll Interval
    precision : Int8;    // Precision

    rootDelay             : FixedPoint16.16;  // Root Delay
    rootDispersion        : FixedPoint16.16;  // Root Dispersion
    referenceIdentifier   : UInt32;           // Reference Identifier

    referenceTimestamp    : Timestamp64;      // Reference Timestamp
    originateTimestamp    : Timestamp64;      // Originate Timestamp
    receiveTimestamp      : Timestamp64;      // Receive Timestamp
    transmitTimestamp     : Timestamp64;      // Transmit Timestamp

    extensionField        : []ExtensionField; // Extension fields (optional)
    authenticationField   : AuthenticationField?; // Authentication field (optional)
  };

  type ExtensionField = struct {
    fieldType : UInt16;
    length    : UInt16;
    value     : [UInt8] @length;
  };

  type AuthenticationField = struct {
    keyIdentifier : UInt32;
    digest        : [UInt8] @32; // Assuming SHA-256
  };

  type FixedPoint16.16 = struct {
    integralPart   : UInt16;
    fractionalPart : UInt16;
  };

  type Timestamp64 = struct {
    seconds  : UInt32;
    fraction : UInt32;
  };
}