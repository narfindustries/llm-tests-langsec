protocolVersion: 
  major: uint8
  minor: uint8

random: 
  gmtUnixTime: uint32
  randomBytes: byte[28]

cipherSuite: uint16

compressionMethod: uint8

extensionType: uint16

extension: 
  extensionType: extensionType
  extensionDataLength: uint16
  extensionData: byte

serverName: 
  serverNameType: uint8
  serverNameLength: uint16
  serverName: byte

serverNameList: 
  serverNameListLength: uint16
  serverNames: serverName[serverNameListLength]

supportedGroupKeyShareEntry: 
  group: uint16
  keyExchangeLength: uint16
  keyExchange: byte

keyShareEntry: 
  group: uint16
  keyExchangeLength: uint16
  keyExchange: byte

supportedVersionEntry: 
  version: protocolVersion

clientHello: 
  legacyVersion: protocolVersion
  random: random
  legacySessionIdLength: uint8
  legacySessionId: byte[legacySessionIdLength]
  cipherSuitesLength: uint16
  cipherSuites: cipherSuite[cipherSuitesLength]
  legacyCompressionMethodsLength: uint8
  legacyCompressionMethods: compressionMethod[legacyCompressionMethodsLength]
  extensionsLength: uint16
  extensions: extension[extensionsLength]

format ClientHelloFormat: 
  legacyVersion: protocolVersion
  random: random
  legacySessionIdLength: uint8
  legacySessionId: byte
  cipherSuitesLength: uint16
  cipherSuites: cipherSuite
  legacyCompressionMethodsLength: uint8
  legacyCompressionMethods: compressionMethod
  extensionsLength: uint16
  extensions: extension

pattern ClientHelloPattern: 
  legacyVersion: 2
  random: 32
  legacySessionIdLength: 1
  legacySessionId: legacySessionIdLength
  cipherSuitesLength: 2
  cipherSuites: cipherSuitesLength
  legacyCompressionMethodsLength: 1
  legacyCompressionMethods: legacyCompressionMethodsLength
  extensionsLength: 2
  extensions: extensionsLength

example ClientHelloExample: 
  legacyVersion: 
    major: 3
    minor: 3
  random: 
    gmtUnixTime: 0
    randomBytes: 28
  legacySessionIdLength: 0
  legacySessionId: 0
  cipherSuitesLength: 2
  cipherSuites: [5,9]
  legacyCompressionMethodsLength: 1
  legacyCompressionMethods: [0]
  extensionsLength: 6
  extensions: [
    {
      extensionType: 0
      extensionDataLength: 12
      extensionData: 12
    },
    {
      extensionType: 23
      extensionDataLength: 6
      extensionData: 6
    },
    {
      extensionType: 51
      extensionDataLength: 50
      extensionData: 50
    },
    {
      extensionType: 43
      extensionDataLength: 6
      extensionData: 6
    },
    {
      extensionType: 13
      extensionDataLength: 2
      extensionData: 2
    },
    {
      extensionType: 15
      extensionDataLength: 0
      extensionData: 0
    }
  ]