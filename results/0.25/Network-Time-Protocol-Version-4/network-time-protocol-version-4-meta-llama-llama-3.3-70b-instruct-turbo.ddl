type LeapIndicator = uint(2);
type VersionNumber = uint(3);
type Mode = uint(3);
type Poll = uint(8);
type Precision = int(8);
type RootDelay = uint(32);
type RootDispersion = uint(32);
type RefId = choice {
  ascii_string: string(4),
  ipv4_address: uint(32),
  ipv6_address: uint(32)
};
type Timestamp = uint(64);
type ExtensionType = uint(16);
type ExtensionLength = uint(16);
type ExtensionValue = bytes;

type NTPExtensionFieldTypes = enum {
  reserved = 0,
  authentication = 2,
  ntpv3 = 3,
  ntpv4 = 4,
  md5 = 5,
  sha1 = 6,
  aes128_cm = 7,
  aes256_cm = 8
};

type NTPExtensionFieldValues = choice {
  authentication: struct {
    type: uint(16),
    length: uint(16),
    key_id: uint(32),
    value: bytes(length - 4)
  },
  ntpv3: struct {
    type: uint(16),
    length: uint(16),
    value: bytes(length)
  },
  ntpv4: struct {
    type: uint(16),
    length: uint(16),
    value: bytes(length)
  },
  md5: struct {
    type: uint(16),
    length: uint(16),
    key_id: uint(32),
    value: bytes(length - 4)
  },
  sha1: struct {
    type: uint(16),
    length: uint(16),
    key_id: uint(32),
    value: bytes(length - 4)
  },
  aes128_cm: struct {
    type: uint(16),
    length: uint(16),
    key_id: uint(32),
    value: bytes(length - 4)
  },
  aes256_cm: struct {
    type: uint(16),
    length: uint(16),
    key_id: uint(32),
    value: bytes(length - 4)
  }
};

type NTPExtensionFieldValue = choice {
  reserved: bytes(0),
  authentication: NTPExtensionFieldValues.authentication,
  ntpv3: NTPExtensionFieldValues.ntpv3,
  ntpv4: NTPExtensionFieldValues.ntpv4,
  md5: NTPExtensionFieldValues.md5,
  sha1: NTPExtensionFieldValues.sha1,
  aes128_cm: NTPExtensionFieldValues.aes128_cm,
  aes256_cm: NTPExtensionFieldValues.aes256_cm
};

type NTPExtensionField = struct {
  type: NTPExtensionFieldTypes,
  length: ExtensionLength,
  value: NTPExtensionFieldValue
};

type NTPExtensionFields = array {
  NTPExtensionField
};

type NTPPacket = struct {
  leap_indicator: LeapIndicator,
  version_number: VersionNumber,
  mode: Mode,
  poll: Poll,
  precision: Precision,
  root_delay: RootDelay,
  root_dispersion: RootDispersion,
  ref_id: RefId,
  ref_timestamp: Timestamp,
  orig_timestamp: Timestamp,
  recv_timestamp: Timestamp,
  trans_timestamp: Timestamp,
  extension_fields: NTPExtensionFields
};

type NTPMessage = choice {
  ntp_packet: NTPPacket
};