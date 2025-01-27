module Network.Time.Protocol.Version4 {

  import DAEDALUS;

  record Header {
    li         : UInt3;      // Leap indicator
    vn         : UInt3;      // Version number
    mode       : UInt4;      // Mode
    stratum    : UInt8;      // Stratum level
    poll       : UInt8;      // Poll interval
    precision  : UInt8;      // Precision
    rootDelay  : UInt32;     // Root delay
    rootDispersion : UInt32; // Root dispersion
    refId      : UInt32;     // Reference ID
  }

  record Timestamp {
    seconds    : UInt32;     // Seconds
    fraction   : UInt32;     // Fraction of a second
  }

  type NtpMessage = struct {
    header     : Header;
    refTime    : Timestamp;  // Reference timestamp
    origTime   : Timestamp;  // Origin timestamp
    recvTime   : Timestamp;  // Receive timestamp
    transTime  : Timestamp;  // Transmit timestamp
  };
}