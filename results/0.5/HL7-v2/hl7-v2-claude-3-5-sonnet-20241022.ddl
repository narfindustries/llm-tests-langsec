def Main = Segment_MSH ~ End

def Segment_MSH = "MSH" ~ "|" ~ field_separator ~ encoding_chars ~ sending_app ~ sending_facility ~ receiving_app ~ receiving_facility ~ datetime ~ security ~ message_type ~ message_control_id ~ processing_id ~ version_id

def field_separator = "|"
def encoding_chars = "^~\\&"

def sending_app = (!"|")* ~ "|"
def sending_facility = (!"|")* ~ "|"
def receiving_app = (!"|")* ~ "|"
def receiving_facility = (!"|")* ~ "|"
def datetime = digit{14} ~ "|"
def security = (!"|")* ~ "|"
def message_type = (!"|")* ~ "|"
def message_control_id = (!"|")* ~ "|"
def processing_id = (!"|")* ~ "|"
def version_id = (!"|")* ~ "|"

def digit = '0'..'9'