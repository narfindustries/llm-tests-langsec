ClientHello {
    uint16 ProtocolVersion;
    opaque Random[32];
    uint8 SessionIDLength;
    opaque SessionID[SessionIDLength];
    uint16 CipherSuitesLength;
    uint16 CipherSuites[CipherSuitesLength / 2];
    uint8 CompressionMethodsLength;
    uint8 CompressionMethods[CompressionMethodsLength];
    uint16 ExtensionsLength;
    Extension Extensions[ExtensionsLength] {
        uint16 ExtensionType;
        uint16 ExtensionDataLength;
        switch(ExtensionType) {
            case 0x0000: // server_name
                struct {
                    uint16 NameListLength;
                    struct {
                        uint8 NameType;
                        uint16 HostNameLength;
                        opaque HostName[HostNameLength];
                    } NameList[NameListLength / 3];
                };

            case 0x000a: // supported_groups
                struct {
                    uint16 SupportedGroupsLength;
                    uint16 SupportedGroups[SupportedGroupsLength / 2];
                };

            case 0x000b: // ec_point_formats (legacy, typically not used in TLS 1.3)
                struct {
                    uint8 ECPointFormatsLength;
                    uint8 ECPointFormats[ECPointFormatsLength];
                };

            case 0x000d: // signature_algorithms
                struct {
                    uint16 SignatureAlgorithmsLength;
                    uint16 SignatureAlgorithms[SignatureAlgorithmsLength / 2];
                };

            case 0x002b: // supported_versions
                struct {
                    uint8 SupportedVersionsLength;
                    uint16 SupportedVersions[SupportedVersionsLength / 2];
                };

            case 0x0033: // key_share
                struct {
                    uint16 KeyShareListLength;
                    struct {
                        uint16 Group;
                        uint16 KeyExchangeLength;
                        opaque KeyExchange[KeyExchangeLength];
                    } KeyShareList[KeyShareListLength / 4];
                };

            case 0x002d: // psk_key_exchange_modes
                struct {
                    uint8 PSKKeyExchangeModesLength;
                    uint8 PSKKeyExchangeModes[PSKKeyExchangeModesLength];
                };

            case 0x0031: // pre_shared_key
                struct {
                    uint16 PSKIdentitiesLength;
                    struct {
                        opaque PSKIdentity<1..65535>;
                        uint32 ObfuscatedTicketAge;
                    } PSKIdentities[PSKIdentitiesLength / 4];
                    uint16 PSKBindersLength;
                    opaque PSKBinders[PSKBindersLength] {
                        uint8 PSKBinderLength;
                        opaque PSKBinder[PSKBinderLength];
                    }
                };

            case 0x0010: // application_layer_protocol_negotiation (ALPN)
                struct {
                    uint16 ProtocolNameListLength;
                    struct {
                        uint8 ProtocolNameLength;
                        opaque ProtocolName[ProtocolNameLength];
                    } ProtocolNameList[ProtocolNameListLength / 2];
                };

            case 0x001b: // extension_type for session ticket (legacy, not used in TLS 1.3)
                struct {
                    // Empty body for session ticket in ClientHello
                };

            case 0x0017: // extended_master_secret (legacy, not used in TLS 1.3)
                struct {
                    // Empty body for extended master secret
                };

            case 0x0032: // early_data
                struct {
                    // Empty body for early data indication
                };

            case 0x0039: // certificate_authorities
                struct {
                    uint16 AuthoritiesListLength;
                    struct {
                        uint16 DistinguishedNameLength;
                        opaque DistinguishedName[DistinguishedNameLength];
                    } AuthoritiesList[AuthoritiesListLength / 3];
                };

            case 0x0030: // post_handshake_auth
                struct {
                    // Empty body for post-handshake auth indication
                };

            default:
                opaque ExtensionData[ExtensionDataLength];
        }
    }
}