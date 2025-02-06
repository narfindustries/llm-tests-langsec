type NTPPacket = struct {
    li_vn_mode uint8;
    stratum uint8;
    poll int8;
    precision int8;
    root_delay uint32;
    root_dispersion uint32;
    ref_id uint32;
    ref_timestamp uint64;
    org_timestamp uint64;
    recv_timestamp uint64;
    trans_timestamp uint64;
    extensions []Extension;
    key_id optional uint32;
    message_digest optional bytes;
};

type Extension = struct {
    field_type uint16;
    length uint16;
    value bytes;
};

func parseNTPPacket(input bytes) -> NTPPacket {
    let base = parseBaseNTPPacket(input);
    let exts = parseExtensions(input[48:], base);
    let auth = parseAuthentication(input, exts);
    return NTPPacket{
        li_vn_mode: base.li_vn_mode,
        stratum: base.stratum,
        poll: base.poll,
        precision: base.precision,
        root_delay: base.root_delay,
        root_dispersion: base.root_dispersion,
        ref_id: base.ref_id,
        ref_timestamp: base.ref_timestamp,
        org_timestamp: base.org_timestamp,
        recv_timestamp: base.recv_timestamp,
        trans_timestamp: base.trans_timestamp,
        extensions: exts.extensions,
        key_id: auth.key_id,
        message_digest: auth.message_digest
    };
}

type BaseNTPPacket = struct {
    li_vn_mode uint8;
    stratum uint8;
    poll int8;
    precision int8;
    root_delay uint32;
    root_dispersion uint32;
    ref_id uint32;
    ref_timestamp uint64;
    org_timestamp uint64;
    recv_timestamp uint64;
    trans_timestamp uint64;
};

func parseBaseNTPPacket(input bytes) -> BaseNTPPacket {
    return BaseNTPPacket{
        li_vn_mode: input[0],
        stratum: input[1],
        poll: int8(input[2]),
        precision: int8(input[3]),
        root_delay: uint32(input[4:8]),
        root_dispersion: uint32(input[8:12]),
        ref_id: uint32(input[12:16]),
        ref_timestamp: uint64(input[16:24]),
        org_timestamp: uint64(input[24:32]),
        recv_timestamp: uint64(input[32:40]),
        trans_timestamp: uint64(input[40:48])
    };
}

type ExtensionsResult = struct {
    extensions []Extension;
    next_index int;
};

func parseExtensions(input bytes, base BaseNTPPacket) -> ExtensionsResult {
    var extensions []Extension;
    var index = 0;
    while index + 4 <= len(input) {
        let field_type = uint16(input[index:index+2]);
        let length = uint16(input[index+2:index+4]);
        if index + 4 + length > len(input) {
            break;
        }
        extensions = append(extensions, Extension{
            field_type: field_type,
            length: length,
            value: input[index+4:index+4+length]
        });
        index += 4 + length;
    }
    return ExtensionsResult{
        extensions: extensions,
        next_index: index
    };
}

type AuthResult = struct {
    key_id optional uint32;
    message_digest optional bytes;
};

func parseAuthentication(input bytes, exts ExtensionsResult) -> AuthResult {
    let auth_start = 48 + exts.next_index;
    if len(input) > auth_start + 4 {
        let key_id = uint32(input[auth_start:auth_start+4]);
        let digest_start = auth_start + 4;
        return AuthResult{
            key_id: key_id,
            message_digest: input[digest_start:]
        };
    } else {
        return AuthResult{
            key_id: nil,
            message_digest: nil
        };
    }
}