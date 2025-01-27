module MQTT {
    type Byte = UInt8;

    type FixedHeader = struct {
        message_type   : UInt4;
        dup_flag       : UInt1;
        qos_level      : UInt2;
        retain         : UInt1;
        remaining_length : RemainingLength;
    };

    type RemainingLength = struct {
        more_follow : UInt1;
        value       : UInt7;
        next        : if more_follow == 1 { RemainingLength } else { () };
    };

    type ConnectFlags = struct {
        reserved     : UInt1 = 0;
        clean_session: UInt1;
        will_flag    : UInt1;
        will_qos     : UInt2;
        will_retain  : UInt1;
        password_flag: UInt1;
        username_flag: UInt1;
    };

    type ConnectPayload = struct {
        client_identifier : UTF8String;
        will_topic        : if parent(ConnectFlags).will_flag == 1 { UTF8String } else { () };
        will_message      : if parent(ConnectFlags).will_flag == 1 { UTF8String } else { () };
        username          : if parent(ConnectFlags).username_flag == 1 { UTF8String } else { () };
        password          : if parent(ConnectFlags).password_flag == 1 { UTF8String } else { () };
    };

    type Connect = struct {
        fixed_header : FixedHeader;
        protocol_name: UTF8String;
        version      : UInt8;
        connect_flags: ConnectFlags;
        keep_alive   : UInt16;
        payload      : ConnectPayload;
    };

    type Publish = struct {
        fixed_header : FixedHeader;
        topic_name   : UTF8String;
        packet_id    : if parent(FixedHeader).qos_level > 0 { UInt16 } else { () };
        payload      : [Byte];
    };

    type SubscribeTopic = struct {
        topic_filter : UTF8String;
        qos          : UInt8;
    };

    type Subscribe = struct {
        fixed_header : FixedHeader;
        packet_id    : UInt16;
        topics       : [SubscribeTopic];
    };

    type Unsubscribe = struct {
        fixed_header : FixedHeader;
        packet_id    : UInt16;
        topic_filters: [UTF8String];
    };

    type Message = union {
        connect    : if parent(FixedHeader).message_type == 1 { Connect } else { () };
        publish    : if parent(FixedHeader).message_type == 3 { Publish } else { () };
        subscribe  : if parent(FixedHeader).message_type == 8 { Subscribe } else { () };
        unsubscribe: if parent(FixedHeader).message_type == 10 { Unsubscribe } else { () };
    };

    type MQTT_Packet = struct {
        fixed_header : FixedHeader;
        message      : Message;
    };
}