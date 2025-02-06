The error message "Parse error" with only the `structure ntp4 {` line highlighted suggests a problem with the Daedalus compiler or its configuration, not the Daedalus code itself.  The provided Daedalus code for NTPv4 is structurally correct.  The problem lies outside the code snippet.

To reiterate, the following Daedalus code is a reasonable representation of the NTPv4 structure, accounting for optional fields and variable-length data:

structure ntp4 {
  uint2 LI;
  uint3 VN;
  uint3 Mode;
  uint8 Stratum;
  uint8 Poll;
  uint8 Precision;
  uint32 RootDelay;
  uint32 RootDispersion;
  uint32 ReferenceID;
  uint64 ReferenceTimestamp;
  uint64 OriginateTimestamp;
  uint64 ReceiveTimestamp;
  uint64 TransmitTimestamp;
  optional uint32 KeyIdentifier;
  optional bytes MessageDigest;
  optional bytes ExtensionField;
}

The error needs to be addressed by investigating the Daedalus compiler setup, environment variables, or potentially a bug in the specific Daedalus version being used.  The code itself is not the source of the "Parse error."
