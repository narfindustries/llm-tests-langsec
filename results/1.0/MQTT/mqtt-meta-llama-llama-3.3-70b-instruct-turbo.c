#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define mqtt_packet_type_connect 1
#define mqtt_packet_type_connack 2
#define mqtt_packet_type_publish 3
#define mqtt_packet_type_puback 4
#define mqtt_packet_type_pubrec 5
#define mqtt_packet_type_pubrel 6
#define mqtt_packet_type_pubcomp 7
#define mqtt_packet_type_subscribe 8
#define mqtt_packet_type_suback 9
#define mqtt_packet_type_unsubscribe 10
#define mqtt_packet_type_unsuback 11
#define mqtt_packet_type_pingreq 12
#define mqtt_packet_type_pingresp 13
#define mqtt_packet_type_disconnect 14

typedef enum {
    QOS_AT_MOST_ONCE = 0,
    QOS_AT_LEAST_ONCE = 1,
    QOS_EXACTLY_ONCE = 2
} QoS;

typedef enum {
    CONNECT_SUCCESS = 0,
    CONNECT_UNACCEPTABLE_PROTOCOL_VERSION = 1,
    CONNECT_IDENTIFIER_REJECTED = 2,
    CONNECT_SERVER_UNAVAILABLE = 3,
    CONNECT_UNKNOWN_RETURN_CODE = 4,
    CONNECT_NOT_AUTHORIZED = 5
} ConnectReturnCode;

typedef enum {
    PUBACK_SUCCESS = 0,
    PUBACK_NO_MATCHING_SUBSCRIBERS = 16,
    PUBACK_UNSPECIFIED_ERROR = 17,
    PUBACK_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    PUBACK_NOT_AUTHORIZED = 131,
    PUBACK_TOPIC_NAME_INVALID = 135,
    PUBACK_PACKET_IDENTIFIER_IN_USE = 137,
    PUBACK_QUOTA_EXCEEDED = 140,
    PUBACK_PAYLOAD_FORMAT_INVALID = 144
} PubAckReasonCode;

typedef enum {
    PUBREC_SUCCESS = 0,
    PUBREC_NO_MATCHING_SUBSCRIBERS = 16,
    PUBREC_UNSPECIFIED_ERROR = 17,
    PUBREC_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    PUBREC_NOT_AUTHORIZED = 131,
    PUBREC_TOPIC_NAME_INVALID = 135,
    PUBREC_PACKET_IDENTIFIER_IN_USE = 137,
    PUBREC_QUOTA_EXCEEDED = 140,
    PUBREC_PAYLOAD_FORMAT_INVALID = 144
} PubRecReasonCode;

typedef enum {
    PUBREL_SUCCESS = 0,
    PUBREL_NO_MATCHING_SUBSCRIBERS = 16,
    PUBREL_UNSPECIFIED_ERROR = 17,
    PUBREL_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    PUBREL_NOT_AUTHORIZED = 131,
    PUBREL_TOPIC_NAME_INVALID = 135,
    PUBREL_PACKET_IDENTIFIER_IN_USE = 137,
    PUBREL_QUOTA_EXCEEDED = 140,
    PUBREL_PAYLOAD_FORMAT_INVALID = 144
} PubRelReasonCode;

typedef enum {
    PUBCOMP_SUCCESS = 0,
    PUBCOMP_NO_MATCHING_SUBSCRIBERS = 16,
    PUBCOMP_UNSPECIFIED_ERROR = 17,
    PUBCOMP_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    PUBCOMP_NOT_AUTHORIZED = 131,
    PUBCOMP_TOPIC_NAME_INVALID = 135,
    PUBCOMP_PACKET_IDENTIFIER_IN_USE = 137,
    PUBCOMP_QUOTA_EXCEEDED = 140,
    PUBCOMP_PAYLOAD_FORMAT_INVALID = 144
} PubCompReasonCode;

typedef enum {
    SUBACK_SUCCESS = 0,
    SUBACK_NO_SUBSCRIPTION_EXISTED = 17,
    SUBACK_UNSPECIFIED_ERROR = 18,
    SUBACK_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    SUBACK_NOT_AUTHORIZED = 131,
    SUBACK_TOPIC_NAME_INVALID = 135,
    SUBACK_PACKET_IDENTIFIER_IN_USE = 137,
    SUBACK_QUOTA_EXCEEDED = 140
} SubAckReturnCode;

typedef enum {
    UNSUBACK_SUCCESS = 0,
    UNSUBACK_NO_SUBSCRIPTION_EXISTED = 17,
    UNSUBACK_UNSPECIFIED_ERROR = 18,
    UNSUBACK_IMPLEMENTATION_SPECIFIC_ERROR = 128,
    UNSUBACK_NOT_AUTHORIZED = 131,
    UNSUBACK_TOPIC_NAME_INVALID = 135,
    UNSUBACK_PACKET_IDENTIFIER_IN_USE = 137,
    UNSUBACK_QUOTA_EXCEEDED = 140
} UnsubAckReturnCode;

typedef enum {
    DISCONNECT_NORMAL_DISCONNECTION = 0,
    DISCONNECT_WITH_WILL_MESSAGE = 1,
    DISCONNECT_UNSPECIFIED_ERROR = 2,
    DISCONNECT_MALFORMED_PACKET = 3,
    DISCONNECT_PROTOCOL_ERROR = 4,
    DISCONNECT_IMPLEMENTATION_SPECIFIC_ERROR = 5,
    DISCONNECT_NOT_AUTHORIZED = 128,
    DISCONNECT_SERVER_BUSY = 129,
    DISCONNECT_SERVER_SHUTTING_DOWN = 130,
    DISCONNECT_KEEP_ALIVE_TIMEOUT = 131,
    DISCONNECT_SESSION_TAKEN_OVER = 132,
    DISCONNECT_TOPIC_FILTER_INVALID = 133,
    DISCONNECT_TOPIC_NAME_INVALID = 134,
    DISCONNECT_RECEIVE_MAXIMUM_EXCEEDED = 135,
    DISCONNECT_TOPIC_ALIAS_INVALID = 136,
    DISCONNECT_PACKET_TOO_LARGE = 137,
    DISCONNECT_MESSAGE_RATE_TOO_HIGH = 138,
    DISCONNECT_QUOTA_EXCEEDED = 139,
    DISCONNECT_ADMINISTRATIVE_ACTION = 140,
    DISCONNECT_PAYLOAD_FORMAT_INVALID = 141,
    DISCONNECT_RETENTION_INVALID = 142,
    DISCONNECT_CONNECTION_RATE_EXCEEDED = 143,
    DISCONNECT_CONNECTION_LIMIT_EXCEEDED = 145
} DisconnectReasonCode;

typedef struct {
    uint8_t packet_type;
    uint8_t flags;
    uint16_t remaining_length;
    uint8_t* payload;
} MqttPacket;

typedef struct {
    MqttPacket packet;
    char* protocol_name;
    uint8_t protocol_level;
    uint8_t connect_flags;
    uint16_t keep_alive;
    uint32_t property_length;
    void* properties;
    char* client_id;
    char* will_topic;
    uint8_t* will_payload;
    char* username;
    uint8_t* password;
} ConnectPacket;

typedef struct {
    MqttPacket packet;
    uint8_t session_present;
    uint8_t return_code;
    uint32_t property_length;
    void* properties;
} ConnAckPacket;

typedef struct {
    MqttPacket packet;
    char* topic_name;
    uint16_t packet_id;
    uint32_t property_length;
    void* properties;
    uint8_t* payload;
} PublishPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint8_t reason_code;
    uint32_t property_length;
    void* properties;
} PubAckPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint8_t reason_code;
    uint32_t property_length;
    void* properties;
} PubRecPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint8_t reason_code;
    uint32_t property_length;
    void* properties;
} PubRelPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint8_t reason_code;
    uint32_t property_length;
    void* properties;
} PubCompPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint32_t property_length;
    void* properties;
} SubscribePacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint32_t property_length;
    void* properties;
    uint8_t* return_codes;
} SubAckPacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint32_t property_length;
    void* properties;
} UnsubscribePacket;

typedef struct {
    MqttPacket packet;
    uint16_t packet_id;
    uint32_t property_length;
    void* properties;
    uint8_t* return_codes;
} UnsubAckPacket;

typedef struct {
    MqttPacket packet;
} PingReqPacket;

typedef struct {
    MqttPacket packet;
} PingRespPacket;

typedef struct {
    MqttPacket packet;
    uint8_t reason_code;
    uint32_t property_length;
    void* properties;
} DisconnectPacket;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error: Could not open file '%s'\n", argv[1]);
        return 1;
    }

    uint8_t* buffer = malloc(1024);
    size_t bytes_read = fread(buffer, 1, 1024, input_file);
    fclose(input_file);

    HParser* parser = h_token((const uint8_t*) "MQTT", 4);
    HParser* parser2 = h_uint8();
    HParser* parser3 = h_uint16();
    HParser* parser4 = h_bytes(0, 1024);

    HParser* connect_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* conn_ack_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* publish_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* pub_ack_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* pub_rec_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* pub_rel_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* pub_comp_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* subscribe_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* sub_ack_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* unsubscribe_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* unsub_ack_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* ping_req_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* ping_resp_packet = h_sequence(parser, parser2, parser3, parser4);
    HParser* disconnect_packet = h_sequence(parser, parser2, parser3, parser4);

    HParser* vtable = h_choice(
        connect_packet,
        conn_ack_packet,
        publish_packet,
        pub_ack_packet,
        pub_rec_packet,
        pub_rel_packet,
        pub_comp_packet,
        subscribe_packet,
        sub_ack_packet,
        unsubscribe_packet,
        unsub_ack_packet,
        ping_req_packet,
        ping_resp_packet,
        disconnect_packet
    );

    HParseResult* result = h_parse(vtable, buffer, bytes_read);

    printf("%d\n", (int)result->status);

    h_free_parse_result(result);

    free(buffer);

    return 0;
}