def Main = Grammar {
    def CRLF = '\r' '\n'
    def SP = ' '
    def HTAB = '\t'
    def WSP = SP | HTAB
    def DIGIT = '0'..'9'
    def ALPHA = 'a'..'z' | 'A'..'Z'
    def VCHAR = '\x21'..'\x7E'
    def OWS = $FoldMany(WSP)

    def Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE" | "CONNECT"
    def HTTPVersion = "HTTP/" DIGIT '.' DIGIT

    def FieldContent = (VCHAR | SP | HTAB)+
    def FieldName = (ALPHA | DIGIT | '-')+
    def HeaderField = @name:FieldName ':' OWS @value:FieldContent OWS CRLF

    def RequestLine = @method:Method SP @uri:UriPath SP @version:HTTPVersion CRLF

    def UriPath = ('/' | ALPHA | DIGIT | '-' | '.' | '_' | '~' | '%' | '&' | '=' | ';')+

    def Headers = $Many(HeaderField)

    def MessageBody = $FoldMany(VCHAR | WSP | CRLF)

    def Request = {
        @request_line:RequestLine
        @headers:Headers
        CRLF
        @body:MessageBody?
    }

    Request
}