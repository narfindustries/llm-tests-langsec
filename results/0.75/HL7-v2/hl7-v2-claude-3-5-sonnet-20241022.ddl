grammar HL7v2;

message = header segments*;

header = 'MSH' '|' encoding_chars '|' sending_app '|' sending_fac '|' recv_app '|' recv_fac '|' datetime '|' security '|' msg_type '|' msg_id '|' processing_id '|' version '|' seq_num '|' cont_ptr '|' ack_type '|' app_ack '|' country '|' charset;

segments = segment_id fields+;

segment_id = 'PID' | 'PV1' | 'OBR' | 'OBX' | 'EVN' | 'NK1' | 'IN1';

fields = field ('|' field)*;

field = components ('~' components)*;

components = subcomponents ('^' subcomponents)*;

subcomponents = value ('&' value)*;

encoding_chars = component_sep repeat_sep escape subcomp_sep;

component_sep = '^';
repeat_sep = '~'; 
escape = '\\';
subcomp_sep = '&';

msg_type = msg_code '^' trigger_event '^' msg_structure;

datetime = YYYY MM DD HH mm ss;

value = [^|^~\&]+;

sending_app = value;
sending_fac = value;
recv_app = value;
recv_fac = value;
security = value;
msg_code = value;
trigger_event = value;
msg_structure = value;
msg_id = value;
processing_id = value;
version = value;
seq_num = [0-9]+;
cont_ptr = value;
ack_type = value;
app_ack = value;
country = value;
charset = value;

YYYY = [0-9]{4};
MM = [0-9]{2};
DD = [0-9]{2};
HH = [0-9]{2};
mm = [0-9]{2};
ss = [0-9]{2};