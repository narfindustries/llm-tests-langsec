module Network.Time.Protocol.Version4 {
  import Network.Time.Protocol.Common;

  type NTPHeader = struct {
    li           : uint(2);  // Leap Indicator
    vn           : uint(3);  // Version Number
    mode         : uint(3);  // Mode
    stratum      : uint(8);  // Stratum
    poll         : int(8);   // Poll Interval
    precision    : int(8);   // Precision

    rootDelay        : NTPShortFormat;  // Total round trip delay
    rootDispersion   : NTPShortFormat;  // Max error relative to primary clock
    refId            : uint(32);        // Reference clock identifier

    refTimestamp     : NTPTimestamp;    // Time when the system clock was last set or corrected
    origTimestamp    : NTPTimestamp;    // Time at the client when the request departed for the server
    recvTimestamp    : NTPTimestamp;    // Time at the server when the request arrived from the client
    transTimestamp   : NTPTimestamp;    // Time at the server when the response left for the client

    extensionFields  : [NTPExtensionField] until eof;
  };

  type NTPShortFormat = struct {
    integerPart  : uint(16);
    fractionPart : uint(16);
  };

  type NTPTimestamp = struct {
    seconds      : uint(32);
    fraction     : uint(32);
  };

  type NTPExtensionField = struct {
    fieldType    : uint(16);
    length       : uint(16);
    value        : bytes(this.length);
  };
}