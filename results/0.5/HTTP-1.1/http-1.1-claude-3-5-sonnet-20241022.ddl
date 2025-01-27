def Main = {
    HTTP_message
}

def HTTP_message = {
    Request | Response
}

def Request = {
    Request_line
    *Header_field
    CRLF
    @optional Message_body
}

def Response = {
    Status_line
    *Header_field
    CRLF
    @optional Message_body
}

def Request_line = {
    Method SP Request_target SP HTTP_version CRLF
}

def Status_line = {
    HTTP_version SP Status_code SP Reason_phrase CRLF
}

def Method = {
    $regex "[A-Z]+"
}

def Request_target = {
    $regex "[^ \t\r\n]+"
}

def HTTP_version = {
    "HTTP/" Major "." Minor
}

def Major = {
    $regex "[0-9]+"
}

def Minor = {
    $regex "[0-9]+"
}

def Status_code = {
    $regex "[0-9]{3}"
}

def Reason_phrase = {
    $regex "[^\r\n]*"
}

def Header_field = {
    Field_name ":" @optional OWS Field_value OWS CRLF
}

def Field_name = {
    $regex "[!#$%&'*+.^_`|~0-9A-Za-z-]+"
}

def Field_value = {
    $regex "[^\r\n]*"
}

def Message_body = {
    $regex ".*"
}

def OWS = {
    $regex "[ \t]*"
}

def SP = {
    " "
}

def CRLF = {
    "\r\n"
}