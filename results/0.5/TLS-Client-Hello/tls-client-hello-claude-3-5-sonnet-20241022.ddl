def TLSClientHello = {
    legacy_version: uint16,
    random: byte[32],
    legacy_session_id_length: uint8,
    legacy_session_id: byte[legacy_session_id_length],
    cipher_suites_length: uint16,
    cipher_suites: uint16[cipher_suites_length div 2],
    legacy_compression_methods_length: uint8,
    legacy_compression_methods: byte[legacy_compression_methods_length],
    extensions_length: uint16,
    extensions: Extension[]
}

def Extension = {
    extension_type: uint16,
    extension_length: uint16,
    extension_data: ExtensionData(extension_type, extension_length)
}

def ExtensionData(type: uint16, length: uint16) = {
    if type == 0x002b then SupportedVersions(length)
    else if type == 0x000d then SignatureAlgorithms(length)
    else if type == 0x000a then SupportedGroups(length)
    else if type == 0x0033 then KeyShare(length)
    else if type == 0x0000 then ServerName(length)
    else if type == 0x002a then EarlyData(length)
    else if type == 0x002d then PSKKeyExchangeModes(length)
    else if type == 0x0029 then PreSharedKey(length)
    else if type == 0x002c then Cookie(length)
    else if type == 0x002f then CertificateAuthorities(length)
    else if type == 0x0031 then PostHandshakeAuth(length)
    else if type == 0x0015 then Padding(length)
    else byte[length]
}

def SupportedVersions(length: uint16) = {
    versions_length: uint8,
    versions: uint16[versions_length div 2]
}

def SignatureAlgorithms(length: uint16) = {
    supported_signature_algorithms_length: uint16,
    supported_signature_algorithms: uint16[supported_signature_algorithms_length div 2]
}

def SupportedGroups(length: uint16) = {
    supported_groups_length: uint16,
    supported_groups: uint16[supported_groups_length div 2]
}

def KeyShare(length: uint16) = {
    client_shares_length: uint16,
    client_shares: KeyShareEntry[]
}

def KeyShareEntry = {
    group: uint16,
    key_exchange_length: uint16,
    key_exchange: byte[key_exchange_length]
}

def ServerName(length: uint16) = {
    server_name_list_length: uint16,
    server_name_list: ServerNameEntry[]
}

def ServerNameEntry = {
    name_type: uint8,
    server_name_length: uint16,
    server_name: byte[server_name_length]
}

def EarlyData(length: uint16) = {
    # Empty in ClientHello
}

def PSKKeyExchangeModes(length: uint16) = {
    ke_modes_length: uint8,
    ke_modes: uint8[ke_modes_length]
}

def PreSharedKey(length: uint16) = {
    identities_length: uint16,
    identities: PSKIdentity[],
    binders_length: uint16,
    binders: PSKBinder[]
}

def PSKIdentity = {
    identity_length: uint16,
    identity: byte[identity_length],
    obfuscated_ticket_age: uint32
}

def PSKBinder = {
    binder_length: uint8,
    binder: byte[binder_length]
}

def Cookie(length: uint16) = {
    cookie: byte[length]
}

def CertificateAuthorities(length: uint16) = {
    authorities_length: uint16,
    authorities: DistinguishedName[]
}

def DistinguishedName = {
    dn_length: uint16,
    dn_data: byte[dn_length]
}

def PostHandshakeAuth(length: uint16) = {
    # Empty extension
}

def Padding(length: uint16) = {
    padding: byte[length]
}