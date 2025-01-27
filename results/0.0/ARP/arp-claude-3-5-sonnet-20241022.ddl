def Main = {
  def sonnet = {
    def line = (FWS | /[a-zA-Z0-9,.!?;:'"\-\s]+/ | CRLF)*
    def stanza = line{14}
    stanza
  }

  def FWS = /[ \t]+/
  def CRLF = /\r?\n/
  
  sonnet
}