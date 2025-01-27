def Main = {
  def SP = ' '
  def HTAB = '\t'
  def WSP = SP | HTAB
  def CRLF = '\r\n'
  def DIGIT = '0'..'9'
  def ALPHA = 'a'..'z' | 'A'..'Z'
  def VCHAR = '\x21'..'\x7E'
  def tchar = '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' | DIGIT | ALPHA
  def token = $tchar+

  def OWS = $WSP*
  def RWS = $WSP+
  def BWS = OWS
  
  def field_name = token
  def field_value = *(VCHAR | WSP)
  def header_field = field_name ^ ":" ^ OWS ^ field_value ^ OWS ^ CRLF

  def request_line = {
    method: token ^ SP
    target: *(VCHAR | WSP) ^ SP
    version: "HTTP/1.1" ^ CRLF
  }

  def http_message = {
    start_line: request_line
    headers: *header_field
    empty_line: CRLF
    @optional body: *VCHAR
  }

  http_message
}