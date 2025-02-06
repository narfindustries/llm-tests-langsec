seq ntpv4_packet = 
  ntpv4_header: 
    uint2 leapIndicator,
    uint3 versionNumber,
    uint3 mode,
    uint8 poll,
    uint8 precision,
    int32 rootDelay,
    uint32 rootDispersion,
    bytes refId size 4,
    uint64 referenceTimestamp,
    uint64 originTimestamp,
    uint64 receiveTimestamp,
    uint64 transmitTimestamp
  ;
  ntpv4_extensions: 
    array ntpv4_extension_field
  ;

seq ntpv4_extension_field = 
  uint16 type,
  uint16 length,
  bytes value size length
  ;

seq ntpv4_mac_extension_field = 
  uint16 type value 0x0104,
  uint16 length value 20,
  bytes value size 20
  ;

seq ntpv4_aes_extension_field = 
  uint16 type value 0x0204,
  uint16 length value 20,
  bytes value size 20
  ;

seq ntpv4_autokey_extension_field = 
  uint16 type value 0x0304,
  uint16 length,
  bytes value size length
  ;

seq ntpv4_ntpv4_extension_field = 
  uint16 type value 0x0404,
  uint16 length,
  bytes value size length
  ;

constraint ntpv4_packet {
  for (extension in ntpv4_extensions) {
    switch (extension.type) {
      case 0x0104: 
        extension as ntpv4_mac_extension_field
      case 0x0204: 
        extension as ntpv4_aes_extension_field
      case 0x0304: 
        extension as ntpv4_autokey_extension_field
      case 0x0404: 
        extension as ntpv4_ntpv4_extension_field
    }
  }
}