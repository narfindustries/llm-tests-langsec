#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char field_separator;
    char encoding_characters[4];
    char sending_application[256];
    char sending_facility[256];
    char receiving_application[256];
    char receiving_facility[256];
    char date[9];
    char time[9];
    char message_type[256];
    char message_control_id[256];
    char processing_id[256];
    char version_id[256];
} MSH;

typedef struct {
    char event_type_code[256];
    char date[9];
    char time[9];
    char event_reason_code[256];
} EVN;

typedef struct {
    char set_id[256];
    char patient_id[256];
    char patient_id_internal[256];
    char alternate_patient_id[256];
    char patient_name[256];
    char mothers_maiden_name[256];
    char date_of_birth[9];
    char sex[256];
    char patient_alias[256];
    char race[256];
    char patient_address[256];
    char country_code[256];
    char phone_number[256];
    char business_phone_number[256];
    char contact_persons_name[256];
    char contact_persons_phone_number[256];
    char contact_persons_business_phone_number[256];
    char contact_persons_address[256];
} PID;

typedef struct {
    char set_id[256];
    char visit_number[256];
    char patient_class[256];
    char visit_reason[256];
    char admission_type[256];
    char pre_admit_number[256];
    char prior_patient_location[256];
    char admission_date[9];
    char admission_time[9];
    char discharge_date[9];
    char discharge_time[9];
    char discharge_disposition[256];
    char discharge_diagnosis[256];
    char visit_diagnosis[256];
    char procedure[256];
} PV1;

typedef struct {
    char order_control[256];
    char placer_order_number[256];
    char filler_order_number[256];
    char order_status[256];
    char response_flag[256];
    char quantity_timing[256];
} ORC;

typedef struct {
    char set_id[256];
    char placer_order_number[256];
    char filler_order_number[256];
    char universal_service_id[256];
    char priority[256];
    char requested_date[9];
    char requested_time[9];
    char observation_date[9];
    char observation_time[9];
    char collection_volume[256];
    char collector_id[256];
    char specimen_action_code[256];
    char danger_code[256];
} OBR;

typedef struct {
    char set_id[256];
    char value_type[256];
    char observation_id[256];
    char observation_sub_id[256];
    char observation_value[256];
    char units[256];
    char reference_range[256];
    char abnormal_flags[256];
    char probability[256];
    char nature_of_abnormal_test[256];
    char observation_result_status[256];
} OBX;

#define MSH_SEG_SIZE (256 * 12 + 4 + 9 + 9)
#define EVN_SEG_SIZE (256 + 9 + 9 + 256)
#define PID_SEG_SIZE (256 * 19)
#define PV1_SEG_SIZE (256 * 15)
#define ORC_SEG_SIZE (256 * 6)
#define OBR_SEG_SIZE (256 * 13)
#define OBX_SEG_SIZE (256 * 11)

MSH parse_msh(char *buffer) {
    MSH msh;
    msh.field_separator = buffer[0];
    memcpy(msh.encoding_characters, buffer + 1, 3);
    memcpy(msh.sending_application, buffer + 4, 256);
    memcpy(msh.sending_facility, buffer + 260, 256);
    memcpy(msh.receiving_application, buffer + 516, 256);
    memcpy(msh.receiving_facility, buffer + 772, 256);
    memcpy(msh.date, buffer + 1028, 9);
    memcpy(msh.time, buffer + 1037, 9);
    memcpy(msh.message_type, buffer + 1046, 256);
    memcpy(msh.message_control_id, buffer + 1302, 256);
    memcpy(msh.processing_id, buffer + 1558, 256);
    memcpy(msh.version_id, buffer + 1814, 256);
    return msh;
}

EVN parse_evn(char *buffer) {
    EVN evn;
    memcpy(evn.event_type_code, buffer, 256);
    memcpy(evn.date, buffer + 256, 9);
    memcpy(evn.time, buffer + 265, 9);
    memcpy(evn.event_reason_code, buffer + 274, 256);
    return evn;
}

PID parse_pid(char *buffer) {
    PID pid;
    memcpy(pid.set_id, buffer, 256);
    memcpy(pid.patient_id, buffer + 256, 256);
    memcpy(pid.patient_id_internal, buffer + 512, 256);
    memcpy(pid.alternate_patient_id, buffer + 768, 256);
    memcpy(pid.patient_name, buffer + 1024, 256);
    memcpy(pid.mothers_maiden_name, buffer + 1280, 256);
    memcpy(pid.date_of_birth, buffer + 1536, 9);
    memcpy(pid.sex, buffer + 1545, 256);
    memcpy(pid.patient_alias, buffer + 1801, 256);
    memcpy(pid.race, buffer + 2057, 256);
    memcpy(pid.patient_address, buffer + 2313, 256);
    memcpy(pid.country_code, buffer + 2569, 256);
    memcpy(pid.phone_number, buffer + 2825, 256);
    memcpy(pid.business_phone_number, buffer + 3081, 256);
    memcpy(pid.contact_persons_name, buffer + 3337, 256);
    memcpy(pid.contact_persons_phone_number, buffer + 3593, 256);
    memcpy(pid.contact_persons_business_phone_number, buffer + 3849, 256);
    memcpy(pid.contact_persons_address, buffer + 4105, 256);
    return pid;
}

PV1 parse_pv1(char *buffer) {
    PV1 pv1;
    memcpy(pv1.set_id, buffer, 256);
    memcpy(pv1.visit_number, buffer + 256, 256);
    memcpy(pv1.patient_class, buffer + 512, 256);
    memcpy(pv1.visit_reason, buffer + 768, 256);
    memcpy(pv1.admission_type, buffer + 1024, 256);
    memcpy(pv1.pre_admit_number, buffer + 1280, 256);
    memcpy(pv1.prior_patient_location, buffer + 1536, 256);
    memcpy(pv1.admission_date, buffer + 1792, 9);
    memcpy(pv1.admission_time, buffer + 1801, 9);
    memcpy(pv1.discharge_date, buffer + 1810, 9);
    memcpy(pv1.discharge_time, buffer + 1819, 9);
    memcpy(pv1.discharge_disposition, buffer + 1828, 256);
    memcpy(pv1.discharge_diagnosis, buffer + 2084, 256);
    memcpy(pv1.visit_diagnosis, buffer + 2340, 256);
    memcpy(pv1.procedure, buffer + 2596, 256);
    return pv1;
}

ORC parse_orc(char *buffer) {
    ORC orc;
    memcpy(orc.order_control, buffer, 256);
    memcpy(orc.placer_order_number, buffer + 256, 256);
    memcpy(orc.filler_order_number, buffer + 512, 256);
    memcpy(orc.order_status, buffer + 768, 256);
    memcpy(orc.response_flag, buffer + 1024, 256);
    memcpy(orc.quantity_timing, buffer + 1280, 256);
    return orc;
}

OBR parse_obr(char *buffer) {
    OBR obr;
    memcpy(obr.set_id, buffer, 256);
    memcpy(obr.placer_order_number, buffer + 256, 256);
    memcpy(obr.filler_order_number, buffer + 512, 256);
    memcpy(obr.universal_service_id, buffer + 768, 256);
    memcpy(obr.priority, buffer + 1024, 256);
    memcpy(obr.requested_date, buffer + 1280, 9);
    memcpy(obr.requested_time, buffer + 1289, 9);
    memcpy(obr.observation_date, buffer + 1298, 9);
    memcpy(obr.observation_time, buffer + 1307, 9);
    memcpy(obr.collection_volume, buffer + 1316, 256);
    memcpy(obr.collector_id, buffer + 1572, 256);
    memcpy(obr.specimen_action_code, buffer + 1828, 256);
    memcpy(obr.danger_code, buffer + 2084, 256);
    return obr;
}

OBX parse_obx(char *buffer) {
    OBX obx;
    memcpy(obx.set_id, buffer, 256);
    memcpy(obx.value_type, buffer + 256, 256);
    memcpy(obx.observation_id, buffer + 512, 256);
    memcpy(obx.observation_sub_id, buffer + 768, 256);
    memcpy(obx.observation_value, buffer + 1024, 256);
    memcpy(obx.units, buffer + 1280, 256);
    memcpy(obx.reference_range, buffer + 1536, 256);
    memcpy(obx.abnormal_flags, buffer + 1792, 256);
    memcpy(obx.probability, buffer + 2048, 256);
    memcpy(obx.nature_of_abnormal_test, buffer + 2304, 256);
    memcpy(obx.observation_result_status, buffer + 2560, 256);
    return obx;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *buffer = (char *)malloc(file_size);
    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read < file_size) {
        printf("Error reading file: %s\n", argv[1]);
        return 1;
    }

    MSH msh = parse_msh(buffer);
    printf("MSH: %s %s %s %s %s %s %s %s %s %s %s %s %s\n", msh.field_separator, msh.encoding_characters, msh.sending_application, msh.sending_facility, msh.receiving_application, msh.receiving_facility, msh.date, msh.time, msh.message_type, msh.message_control_id, msh.processing_id, msh.version_id);

    EVN evn = parse_evn(buffer + MSH_SEG_SIZE);
    printf("EVN: %s %s %s %s\n", evn.event_type_code, evn.date, evn.time, evn.event_reason_code);

    PID pid = parse_pid(buffer + MSH_SEG_SIZE + EVN_SEG_SIZE);
    printf("PID: %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n", pid.set_id, pid.patient_id, pid.patient_id_internal, pid.alternate_patient_id, pid.patient_name, pid.mothers_maiden_name, pid.date_of_birth, pid.sex, pid.patient_alias, pid.race, pid.patient_address, pid.country_code, pid.phone_number, pid.business_phone_number, pid.contact_persons_name, pid.contact_persons_phone_number, pid.contact_persons_business_phone_number, pid.contact_persons_address);

    PV1 pv1 = parse_pv1(buffer + MSH_SEG_SIZE + EVN_SEG_SIZE + PID_SEG_SIZE);
    printf("PV1: %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n", pv1.set_id, pv1.visit_number, pv1.patient_class, pv1.visit_reason, pv1.admission_type, pv1.pre_admit_number, pv1.prior_patient_location, pv1.admission_date, pv1.admission_time, pv1.discharge_date, pv1.discharge_time, pv1.discharge_disposition, pv1.discharge_diagnosis, pv1.visit_diagnosis, pv1.procedure);

    ORC orc = parse_orc(buffer + MSH_SEG_SIZE + EVN_SEG_SIZE + PID_SEG_SIZE + PV1_SEG_SIZE);
    printf("ORC: %s %s %s %s %s %s\n", orc.order_control, orc.placer_order_number, orc.filler_order_number, orc.order_status, orc.response_flag, orc.quantity_timing);

    OBR obr = parse_obr(buffer + MSH_SEG_SIZE + EVN_SEG_SIZE + PID_SEG_SIZE + PV1_SEG_SIZE + ORC_SEG_SIZE);
    printf("OBR: %s %s %s %s %s %s %s %s %s %s %s %s %s\n", obr.set_id, obr.placer_order_number, obr.filler_order_number, obr.universal_service_id, obr.priority, obr.requested_date, obr.requested_time, obr.observation_date, obr.observation_time, obr.collection_volume, obr.collector_id, obr.specimen_action_code, obr.danger_code);

    OBX obx = parse_obx(buffer + MSH_SEG_SIZE + EVN_SEG_SIZE + PID_SEG_SIZE + PV1_SEG_SIZE + ORC_SEG_SIZE + OBR_SEG_SIZE);
    printf("OBX: %s %s %s %s %s %s %s %s %s %s %s %s\n", obx.set_id, obx.value_type, obx.observation_id, obx.observation_sub_id, obx.observation_value, obx.units, obx.reference_range, obx.abnormal_flags, obx.probability, obx.nature_of_abnormal_test, obx.observation_result_status);

    free(buffer);
    return 0;
}