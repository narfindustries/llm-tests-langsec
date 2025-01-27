HTTP-1.1-spec = {
    // Header parsing helpers
    is_token_char = (
        | is_alpha
        | is_digit
        | is_in("!#$%&'*+-.^_`|~")
    );

    token = +is_token_char;

    // Basic character classes
    is_whitespace = is_in(" \t");
    is_line_end = is_in("\r\n");
    optional_whitespace = *is_whitespace;

    // HTTP version parsing
    http_version = (
        "HTTP/" 
        digit+ "." digit+
    );

    // Status line
    status_line = (
        http_version 
        is_whitespace+ 
        digit{3} 
        is_whitespace+ 
        *(~is_line_end)
        is_line_end
    );

    // Header parsing
    header_name = token;
    header_value = *(~is_line_end);
    header = (
        header_name 
        ":" 
        optional_whitespace 
        header_value 
        is_line_end
    );

    // Headers collection
    headers = *(header);
    end_headers = is_line_end;

    // Full HTTP response
    http_response = (
        status_line 
        headers 
        end_headers
    );
};