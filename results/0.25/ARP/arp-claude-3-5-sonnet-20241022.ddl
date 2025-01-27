def Main = {
  def line = (FWS | CRLF)*
  def word = {c : uint8 where (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')}+
  def punct = {c : uint8 where c == '.' or c == ',' or c == ';' or c == ':' or c == '!' or c == '?'}
  def space = {c : uint8 where c == ' '}
  def FWS = space+
  def CRLF = Match "\r\n"
  
  def sonnetLine = (word (space | punct)*)* CRLF
  def sonnet = sonnetLine{14}
  
  sonnet
}