dialect "HTTP-1.1";

start = message;

message = struct {
    requestline request_line;
    headerlines headers;
    emptyline;
    [body] byte[];  // Optional body
};

request_line = struct {
    method string[] (separator: " "); 
    uri string[] (separator: " ");
    version string[] (separator: "\r\n");
};

headerline = struct {
    name string[] (separator: ":"); 
    value string[] (separator: "\r\n");
};

headerlines = array<headerline> (terminator: "\r\n\r\n");

emptyline = "\r\n";