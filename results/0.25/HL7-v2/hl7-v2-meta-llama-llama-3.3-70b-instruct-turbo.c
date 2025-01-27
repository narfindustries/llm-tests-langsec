#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define an enum for the HL7 message types
typedef enum {
    HL7_MESSAGE_TYPE_MSH = 0,
    HL7_MESSAGE_TYPE_EVN,
    HL7_MESSAGE_TYPE_PID,
    HL7_MESSAGE_TYPE_PV1,
    HL7_MESSAGE_TYPE_OBR,
    HL7_MESSAGE_TYPE_ORC,
    HL7_MESSAGE_TYPE_RXO,
    HL7_MESSAGE_TYPE_RXR,
    HL7_MESSAGE_TYPE_RXC,
    HL7_MESSAGE_TYPE_RXE,
    HL7_MESSAGE_TYPE_RXD,
    HL7_MESSAGE_TYPE.nt,
} hl7_message_type_t;

// Define a struct for the HL7 MSH segment
typedef struct {
    char message_type[4];
    char trigger_event[4];
    char message_control_id[20];
    char processing_id[4];
    char version_id[12];
    char sequence_number[15];
    char continuation_pointer[180];
    char accept_acknowledgment_type[2];
    char application_acknowledgment_type[2];
    char country_code[2];
    char character_set[15];
    char principal_language_of_message[15];
} hl7_msh_segment_t;

// Define a struct for the HL7 EVN segment
typedef struct {
    char event_type_code[4];
    char recorded_date_time[26];
    char date_time_planned_event[26];
    char event_reason_code[4];
    char operator_id[30];
    char event_facility[30];
} hl7_env_segment_t;

// Define a struct for the HL7 PID segment
typedef struct {
    char set_id_pid[4];
    char patient_id[20];
    char patient_identifier_list[250];
    char alternate_patient_id_pi[20];
    char patient_name[250];
    char mother_s_maiden_name[250];
    char date_of_birth[8];
    char sex[1];
    char patient_alias[250];
    char race[40];
    char patient_address[250];
    char county_code[20];
    char phone_number_home[20];
    char phone_number_business[20];
    char primary_language[15];
    char marital_status[1];
    char religion[20];
    char patient_account_number[20];
    char ssn_number_patient[20];
    char driver_s_license_number_patient[20];
    char mother_s_identifier[20];
    char ethnic_group[20];
    char birth_place[250];
    char multiple_birth_indicator[1];
    char birth_order[2];
    char citizenship[20];
    char veteran_s_military_status[2];
    char nationality[20];
    char patient_death_date_and_time[26];
    char patient_death_indicator[1];
} hl7_pid_segment_t;

// Define a struct for the HL7 PV1 segment
typedef struct {
    char set_id_pv1[4];
    char visit_number[20];
    char patient_class[1];
    char visit_number_01[20];
    char admit_reason[60];
    char transfer_reason[60];
    char patient_type[2];
    char visit_description[30];
    char admit_date_time[26];
    char discharge_disposition[2];
    char discharged_to_location[2];
    char diet_type[40];
    char servicing_facility[30];
    char bed_status[2];
    char visit_priority_code[2];
    char pre_admit_number[20];
    char prior_patient_location[30];
    char attending_doctor[30];
    char referring_doctor[30];
    char consulting_doctor[30];
    char visit_number_02[20];
    char patient_valuables[30];
    char patient_valuables_location[30];
} hl7_pv1_segment_t;

// Define a struct for the HL7 OBR segment
typedef struct {
    char set_id_01[4];
    char placment_number[20];
    char filler_order_number[22];
    char universal_service_identifier[90];
    char priority[2];
    char requested_date_time[26];
    char observation_date_time[26];
    char observation_result_status[1];
    char charged_to_location[20];
    char specimen_source[60];
    char specimen_responsible_org[30];
    char specimen_collection_date_time[26];
    char specimen_received_date_time[26];
    char specimen_expiration_date_time[26];
    char processor_sequence_number[5];
    char specimen_container_id[20];
    char specimen_container_type[60];
    char specimen_container_volume[10];
    char specimen_container_units[10];
    char specimen_reference_id[20];
    char specimen_parent_result[20];
    char specimen_collection_method[20];
    char specimen_source_site[60];
} hl7_obr_segment_t;

// Define a struct for the HL7 ORC segment
typedef struct {
    char order_control[2];
    char placer_order_number[22];
    char filler_order_number[22];
    char placer_group_number[20];
    char order_status[2];
    char response_flag[1];
    char quantity_timing[200];
    char parent_result[20];
    char date_time_of_transaction[26];
    char entered_by[30];
    char verified_by[30];
    char ordered_by[30];
    char entered_at_location[30];
    char callback_phone_number[20];
    char order_effective_date_time[26];
    char order_control_code_reason[60];
    char entering_organization[30];
    char entering_device[30];
    char action_by[30];
} hl7_orc_segment_t;

// Define a struct for the HL7 RXO segment
typedef struct {
    char request_date_time[26];
    char requested_by[30];
    char needed_by_date_time[26];
    char verified_by[30];
    char rx_order_status[2];
    char response_flag[1];
    char pharmacy_order_type[2];
    char dispense_as_written_or_product_selection_indicator[1];
    char subscription_number[15];
    char number_of_refills[2];
    char number_of_refills_remaining[2];
    char number_of_packages[3];
    char package_description[20];
} hl7_rxo_segment_t;

// Define a struct for the HL7 RXR segment
typedef struct {
    char route[20];
    char site[20];
    char administration_device[20];
    char administration_method[20];
    char rx_routing[2];
} hl7_rxr_segment_t;

// Define a struct for the HL7 RXC segment
typedef struct {
    char component_type[2];
    char component_code[20];
    char component_amount[10];
    char component_units[10];
    char component_strength[20];
} hl7_rxc_segment_t;

// Define a struct for the HL7 RXE segment
typedef struct {
    char pharmacy_treatment_authorizer[30];
    char dispenser[30];
    char substance_lot_number[20];
    char substance_expiration_date[8];
    char substance_manufacturer_name[30];
    char indirect_indication_flags[2];
    char substance_lot_numberModifier[1];
    char substance_expiration_date_modifier[1];
    char substance_manufacturer_name_modifier[1];
    char intermediary[30];
    char pharmacy_of_most_recent_fill[30];
    char rx_component_type[2];
} hl7_rxe_segment_t;

// Define a struct for the HL7 RXD segment
typedef struct {
    char date_time_of_administration[26];
    char giver_s_identifier[30];
    char administration_note[200];
    char administration_note_type[2];
    char administration_note loài[2];
    char substance_lot_number[20];
    char substance_expiration_date[8];
    char substance_manufacturer_name[30];
    char administration_device[20];
    char administration_method[20];
    char treatment_license_number[15];
    char royal_pharmaceutical_license_number[15];
    char royal_pharmacy_license_number[15];
    char rx_administration_date[8];
} hl7_rxd_segment_t;

// Define a struct for the HL7 message
typedef struct {
    hl7_msh_segment_t msh;
    hl7_env_segment_t evn;
    hl7_pid_segment_t pid;
    hl7_pv1_segment_t pv1;
    hl7_obr_segment_t obr;
    hl7_orc_segment_t orc;
    hl7_rxo_segment_t rxo;
    hl7_rxr_segment_t rxr;
    hl7_rxc_segment_t rxc;
    hl7_rxe_segment_t rxe;
    hl7_rxd_segment_t rxd;
} hl7_message_t;

// Function to parse an HL7 message
hl7_message_t* parse_hl7_message(char* message) {
    hl7_message_t* msg = (hl7_message_t*)malloc(sizeof(hl7_message_t));

    // Parse the MSH segment
    char* msh_segment = strtok(message, "\n");
    if (msh_segment != NULL) {
        strcpy(msg->msh.message_type, strstr(msh_segment, "MSH") + 3);
        strcpy(msg->msh.trigger_event, strstr(msh_segment, "EVN") + 3);
        strcpy(msg->msh.message_control_id, strstr(msh_segment, "MSG") + 3);
        strcpy(msg->msh.processing_id, strstr(msh_segment, "PID") + 3);
        strcpy(msg->msh.version_id, strstr(msh_segment, "VER") + 3);
        strcpy(msg->msh.sequence_number, strstr(msh_segment, "SEQ") + 3);
        strcpy(msg->msh.continuation_pointer, strstr(msh_segment, "CNT") + 3);
        strcpy(msg->msh.accept_acknowledgment_type, strstr(msh_segment, "ACK") + 3);
        strcpy(msg->msh.application_acknowledgment_type, strstr(msh_segment, "APP") + 3);
        strcpy(msg->msh.country_code, strstr(msh_segment, "CCT") + 3);
        strcpy(msg->msh.character_set, strstr(msh_segment, "CHS") + 3);
        strcpy(msg->msh.principal_language_of_message, strstr(msh_segment, "LAN") + 3);
    }

    // Parse the EVN segment
    char* evn_segment = strtok(NULL, "\n");
    if (evn_segment != NULL) {
        strcpy(msg->evn.event_type_code, strstr(evn_segment, "EVN") + 3);
        strcpy(msg->evn.recorded_date_time, strstr(evn_segment, "REPT") + 4);
        strcpy(msg->evn.date_time_planned_event, strstr(evn_segment, "PLNT") + 4);
        strcpy(msg->evn.event_reason_code, strstr(evn_segment, "RSN") + 3);
        strcpy(msg->evn.operator_id, strstr(evn_segment, "OPER") + 4);
        strcpy(msg->evn.event_facility, strstr(evn_segment, "FAC") + 3);
    }

    // Parse the PID segment
    char* pid_segment = strtok(NULL, "\n");
    if (pid_segment != NULL) {
        strcpy(msg->pid.set_id_pid, strstr(pid_segment, "PID") + 3);
        strcpy(msg->pid.patient_id, strstr(pid_segment, "PAT") + 3);
        strcpy(msg->pid.patient_identifier_list, strstr(pid_segment, "ID") + 2);
        strcpy(msg->pid.alternate_patient_id_pi, strstr(pid_segment, "ALT") + 3);
        strcpy(msg->pid.patient_name, strstr(pid_segment, "NAM") + 3);
        strcpy(msg->pid.mother_s_maiden_name, strstr(pid_segment, "MOM") + 3);
        strcpy(msg->pid.date_of_birth, strstr(pid_segment, "DOB") + 3);
        strcpy(msg->pid.sex, strstr(pid_segment, "SEX") + 3);
        strcpy(msg->pid.patient_alias, strstr(pid_segment, "ALI") + 3);
        strcpy(msg->pid.race, strstr(pid_segment, "RAC") + 3);
        strcpy(msg->pid.patient_address, strstr(pid_segment, "ADR") + 3);
        strcpy(msg->pid.county_code, strstr(pid_segment, "CNT") + 3);
        strcpy(msg->pid.phone_number_home, strstr(pid_segment, "HOM") + 3);
        strcpy(msg->pid.phone_number_business, strstr(pid_segment, "BUS") + 3);
        strcpy(msg->pid.primary_language, strstr(pid_segment, "LAN") + 3);
        strcpy(msg->pid.marital_status, strstr(pid_segment, "MAR") + 3);
        strcpy(msg->pid.religion, strstr(pid_segment, "REL") + 3);
        strcpy(msg->pid.patient_account_number, strstr(pid_segment, "ACC") + 3);
        strcpy(msg->pid.ssn_number_patient, strstr(pid_segment, "SSN") + 3);
        strcpy(msg->pid.driver_s_license_number_patient, strstr(pid_segment, "DLN") + 3);
        strcpy(msg->pid.mother_s_identifier, strstr(pid_segment, "MID") + 3);
        strcpy(msg->pid.ethnic_group, strstr(pid_segment, "ETH") + 3);
        strcpy(msg->pid.birth_place, strstr(pid_segment, "BPL") + 3);
        strcpy(msg->pid.multiple_birth_indicator, strstr(pid_segment, "MBI") + 3);
        strcpy(msg->pid.birth_order, strstr(pid_segment, "BOR") + 3);
        strcpy(msg->pid.citizenship, strstr(pid_segment, "CTZ") + 3);
        strcpy(msg->pid.veteran_s_military_status, strstr(pid_segment, "VET") + 3);
        strcpy(msg->pid.nationality, strstr(pid_segment, "NAT") + 3);
        strcpy(msg->pid.patient_death_date_and_time, strstr(pid_segment, "DOD") + 3);
        strcpy(msg->pid.patient_death_indicator, strstr(pid_segment, "DOI") + 3);
    }

    // Parse the PV1 segment
    char* pv1_segment = strtok(NULL, "\n");
    if (pv1_segment != NULL) {
        strcpy(msg->pv1.set_id_pv1, strstr(pv1_segment, "PV1") + 3);
        strcpy(msg->pv1.visit_number, strstr(pv1_segment, "VIS") + 3);
        strcpy(msg->pv1.patient_class, strstr(pv1_segment, "CLAS") + 4);
        strcpy(msg->pv1.visit_number_01, strstr(pv1_segment, "VIS") + 3);
        strcpy(msg->pv1.admit_reason, strstr(pv1_segment, "REAS") + 4);
        strcpy(msg->pv1.transfer_reason, strstr(pv1_segment, "XFR") + 4);
        strcpy(msg->pv1.patient_type, strstr(pv1_segment, "PTY") + 3);
        strcpy(msg->pv1.visit_description, strstr(pv1_segment, "DESC") + 4);
        strcpy(msg->pv1.admit_date_time, strstr(pv1_segment, "ADT") + 3);
        strcpy(msg->pv1.discharge_disposition, strstr(pv1_segment, "DIS") + 3);
        strcpy(msg->pv1.discharged_to_location, strstr(pv1_segment, "DLOC") + 4);
        strcpy(msg->pv1.diet_type, strstr(pv1_segment, "DIET") + 4);
        strcpy(msg->pv1.servicing_facility, strstr(pv1_segment, "SER") + 3);
        strcpy(msg->pv1.bed_status, strstr(pv1_segment, "BED") + 3);
        strcpy(msg->pv1.visit_priority_code, strstr(pv1_segment, "PRIO") + 4);
        strcpy(msg->pv1.pre_admit_number, strstr(pv1_segment, "PRE") + 3);
        strcpy(msg->pv1.prior_patient_location, strstr(pv1_segment, "PRI") + 3);
        strcpy(msg->pv1.attending_doctor, strstr(pv1_segment, "ATD") + 3);
        strcpy(msg->pv1.referring_doctor, strstr(pv1_segment, "REF") + 3);
        strcpy(msg->pv1.consulting_doctor, strstr(pv1_segment, "CON") + 3);
        strcpy(msg->pv1.visit_number_02, strstr(pv1_segment, "VIS") + 3);
        strcpy(msg->pv1.patient_valuables, strstr(pv1_segment, "VAL") + 3);
        strcpy(msg->pv1.patient_valuables_location, strstr(pv1_segment, "LOC") + 3);
    }

    // Parse the OBR segment
    char* obr_segment = strtok(NULL, "\n");
    if (obr_segment != NULL) {
        strcpy(msg->obr.set_id_01, strstr(obr_segment, "OBR") + 3);
        strcpy(msg->obr.placement_number, strstr(obr_segment, "PLM") + 3);
        strcpy(msg->obr.filler_order_number, strstr(obr_segment, "FILL") + 4);
        strcpy(msg->obr.universal_service_identifier, strstr(obr_segment, "SER") + 3);
        strcpy(msg->obr.priority, strstr(obr_segment, "PRI") + 3);
        strcpy(msg->obr.requested_date_time, strstr(obr_segment, "REQ") + 3);
        strcpy(msg->obr.observation_date_time, strstr(obr_segment, "OBS") + 3);
        strcpy(msg->obr.observation_result_status, strstr(obr_segment, "STA") + 3);
        strcpy(msg->obr.charged_to_location, strstr(obr_segment, "CHG") + 3);
        strcpy(msg->obr.specimen_source, strstr(obr_segment, "SRC") + 3);
        strcpy(msg->obr.specimen_responsible_org, strstr(obr_segment, "ORG") + 3);
        strcpy(msg->obr.specimen_collection_date_time, strstr(obr_segment, "COL") + 3);
        strcpy(msg->obr.specimen_received_date_time, strstr(obr_segment, "REC") + 3);
        strcpy(msg->obr.specimen_expiration_date_time, strstr(obr_segment, "EXP") + 3);
        strcpy(msg->obr.processor_sequence_number, strstr(obr_segment, "SEQ") + 3);
        strcpy(msg->obr.specimen_container_id, strstr(obr_segment, "CID") + 3);
        strcpy(msg->obr.specimen_container_type, strstr(obr_segment, "CTY") + 3);
        strcpy(msg->obr.specimen_container_volume, strstr(obr_segment, "VOL") + 3);
        strcpy(msg->obr.specimen_container_units, strstr(obr_segment, "UNT") + 3);
        strcpy(msg->obr.specimen_reference_id, strstr(obr_segment, "REF") + 3);
        strcpy(msg->obr.specimen_parent_result, strstr(obr_segment, "PAR") + 3);
        strcpy(msg->obr.specimen_collection_method, strstr(obr_segment, "MTH") +