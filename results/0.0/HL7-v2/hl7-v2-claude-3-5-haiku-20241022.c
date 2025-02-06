#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* set_id;
    char* patient_id;
    char* patient_identifier_list;
    char* alternate_patient_id;
    char* patient_name;
    char* mothers_maiden_name;
    char* date_of_birth;
    char* administrative_sex;
    char* patient_alias;
} PIDSegment;

typedef struct {
    char* sending_application;
    char* sending_facility;
    char* receiving_application;
    char* receiving_facility;
    char* message_datetime;
    char* security;
    char* message_type;
    char* message_control_id;
    char* processing_id;
    char* version_id;
} MSHSegment;

typedef struct {
    MSHSegment* msh;
    PIDSegment* pid;
} HL7Message;

HParser* create_text_parser() {
    return h_many1(h_ch_range('!', '~'));
}

HParsedToken* parse_msh_segment(const uint8_t* input, size_t length) {
    HParser* sending_app = h_token((const uint8_t*)"MSH", 3);
    HParser* field_separator = h_token((const uint8_t*)"|", 1);
    HParser* text_field = create_text_parser();
    
    HParser* msh_parser = h_sequence(
        sending_app,
        field_separator,
        text_field,  // sending_application
        field_separator,
        text_field,  // sending_facility
        field_separator,
        text_field,  // receiving_application
        field_separator,
        text_field,  // receiving_facility
        field_separator,
        text_field,  // message_datetime
        field_separator,
        text_field,  // security
        field_separator,
        text_field,  // message_type
        field_separator,
        text_field,  // message_control_id
        field_separator,
        text_field,  // processing_id
        field_separator,
        text_field   // version_id
    );
    
    HParseResult* result = h_parse(msh_parser, input, length);
    return result ? result->ast : NULL;
}

HParsedToken* parse_pid_segment(const uint8_t* input, size_t length) {
    HParser* pid_token = h_token((const uint8_t*)"PID", 3);
    HParser* field_separator = h_token((const uint8_t*)"|", 1);
    HParser* text_field = create_text_parser();
    
    HParser* pid_parser = h_sequence(
        pid_token,
        field_separator,
        text_field,  // set_id
        field_separator,
        text_field,  // patient_id
        field_separator,
        text_field,  // patient_identifier_list
        field_separator,
        text_field,  // alternate_patient_id
        field_separator,
        text_field,  // patient_name
        field_separator,
        text_field,  // mothers_maiden_name
        field_separator,
        text_field,  // date_of_birth
        field_separator,
        text_field,  // administrative_sex
        field_separator,
        text_field   // patient_alias
    );
    
    HParseResult* result = h_parse(pid_parser, input, length);
    return result ? result->ast : NULL;
}

char* extract_token_string(HParsedToken* token) {
    if (!token || token->token_type != TT_SEQUENCE) return NULL;
    
    HParsedToken* str_token = (HParsedToken*)token->seq[0];
    if (!str_token || str_token->token_type != TT_BYTES) return NULL;
    
    return strndup((const char*)str_token->bytes, str_token->token_length);
}

HL7Message* parse_hl7_message(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size + 1);
    fread(buffer, 1, file_size, file);
    buffer[file_size] = '\0';

    HL7Message* message = malloc(sizeof(HL7Message));
    message->msh = malloc(sizeof(MSHSegment));
    message->pid = malloc(sizeof(PIDSegment));

    HParsedToken* msh_result = parse_msh_segment(buffer, file_size);
    HParsedToken* pid_result = parse_pid_segment(buffer, file_size);

    if (msh_result && pid_result) {
        // Populate MSH segment
        message->msh->sending_application = extract_token_string((HParsedToken*)msh_result->seq[2]);
        message->msh->sending_facility = extract_token_string((HParsedToken*)msh_result->seq[3]);
        message->msh->receiving_application = extract_token_string((HParsedToken*)msh_result->seq[4]);
        message->msh->receiving_facility = extract_token_string((HParsedToken*)msh_result->seq[5]);
        message->msh->message_datetime = extract_token_string((HParsedToken*)msh_result->seq[6]);
        message->msh->security = extract_token_string((HParsedToken*)msh_result->seq[7]);
        message->msh->message_type = extract_token_string((HParsedToken*)msh_result->seq[8]);
        message->msh->message_control_id = extract_token_string((HParsedToken*)msh_result->seq[9]);
        message->msh->processing_id = extract_token_string((HParsedToken*)msh_result->seq[10]);
        message->msh->version_id = extract_token_string((HParsedToken*)msh_result->seq[11]);

        // Populate PID segment
        message->pid->set_id = extract_token_string((HParsedToken*)pid_result->seq[2]);
        message->pid->patient_id = extract_token_string((HParsedToken*)pid_result->seq[3]);
        message->pid->patient_identifier_list = extract_token_string((HParsedToken*)pid_result->seq[4]);
        message->pid->alternate_patient_id = extract_token_string((HParsedToken*)pid_result->seq[5]);
        message->pid->patient_name = extract_token_string((HParsedToken*)pid_result->seq[6]);
        message->pid->mothers_maiden_name = extract_token_string((HParsedToken*)pid_result->seq[7]);
        message->pid->date_of_birth = extract_token_string((HParsedToken*)pid_result->seq[8]);
        message->pid->administrative_sex = extract_token_string((HParsedToken*)pid_result->seq[9]);
        message->pid->patient_alias = extract_token_string((HParsedToken*)pid_result->seq[10]);
    }

    free(buffer);
    fclose(file);
    return message;
}

void free_hl7_message(HL7Message* message) {
    if (message) {
        if (message->msh) {
            free(message->msh->sending_application);
            free(message->msh->sending_facility);
            free(message->msh->receiving_application);
            free(message->msh->receiving_facility);
            free(message->msh->message_datetime);
            free(message->msh->security);
            free(message->msh->message_type);
            free(message->msh->message_control_id);
            free(message->msh->processing_id);
            free(message->msh->version_id);
            free(message->msh);
        }
        
        if (message->pid) {
            free(message->pid->set_id);
            free(message->pid->patient_id);
            free(message->pid->patient_identifier_list);
            free(message->pid->alternate_patient_id);
            free(message->pid->patient_name);
            free(message->pid->mothers_maiden_name);
            free(message->pid->date_of_birth);
            free(message->pid->administrative_sex);
            free(message->pid->patient_alias);
            free(message->pid);
        }
        
        free(message);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    HL7Message* message = parse_hl7_message(argv[1]);
    
    if (message) {
        printf("Parsed HL7 Message:\n");
        printf("Sending Application: %s\n", message->msh->sending_application);
        printf("Patient Name: %s\n", message->pid->patient_name);
        
        free_hl7_message(message);
    } else {
        fprintf(stderr, "Failed to parse HL7 message\n");
        return 1;
    }

    return 0;
}