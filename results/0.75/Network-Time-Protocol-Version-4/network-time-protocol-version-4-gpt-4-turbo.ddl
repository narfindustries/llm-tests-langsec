module Network.Time.Protocol.Version4 {
  type NTPHeader = struct {
    li           : UInt2;    // Leap Indicator
    vn           : UInt3;    // Version Number
    mode         : UInt3;    // Mode
    stratum      : UInt8;    // Stratum
    poll         : Int8;     // Poll Interval
    precision    : Int8;     // Precision
    rootDelay    : UInt32;   // Root Delay
    rootDisp     : UInt32;   // Root Dispersion
    refId        : UInt32;   // Reference Identifier
    refTimestamp : UInt64;   // Reference Timestamp
    orgTimestamp : UInt64;   // Originate Timestamp
    recTimestamp : UInt64;   // Receive Timestamp
    traTimestamp : UInt64;   // Transmit Timestamp
  };
}
