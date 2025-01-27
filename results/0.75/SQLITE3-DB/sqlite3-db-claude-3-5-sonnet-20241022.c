#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_parser() {
    // Core SQL components
    HParser* whitespace = h_whitespace(h_many);
    HParser* identifier = h_sequence(h_ch_range('a', 'z'), h_many(h_choice(h_ch_range('a', 'z'), h_ch_range('0', '9'), h_ch('_'), NULL)), NULL);
    HParser* number = h_many1(h_ch_range('0', '9'));
    HParser* string_literal = h_sequence(h_ch('\''), h_many(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch(' '), NULL)), h_ch('\''), NULL);
    
    // SQL keywords
    HParser* create = h_token("CREATE", 6);
    HParser* table = h_token("TABLE", 5);
    HParser* insert = h_token("INSERT", 6);
    HParser* into = h_token("INTO", 4);
    HParser* values = h_token("VALUES", 6);
    HParser* select = h_token("SELECT", 6);
    HParser* from = h_token("FROM", 4);
    HParser* where = h_token("WHERE", 5);
    
    // Column definition
    HParser* column_type = h_choice(h_token("INTEGER", 7), h_token("TEXT", 4), h_token("REAL", 4), NULL);
    HParser* column_def = h_sequence(identifier, whitespace, column_type, NULL);
    HParser* column_list = h_sepBy(column_def, h_sequence(h_ch(','), whitespace, NULL));
    
    // Value expressions
    HParser* value = h_choice(number, string_literal, NULL);
    HParser* value_list = h_sepBy(value, h_sequence(h_ch(','), whitespace, NULL));
    
    // Comparison operators
    HParser* operator = h_choice(h_ch('='), h_token(">=", 2), h_token("<=", 2), h_token("<>", 2), h_ch('<'), h_ch('>'), NULL);
    
    // Condition
    HParser* condition = h_sequence(identifier, whitespace, operator, whitespace, value, NULL);
    
    // CREATE TABLE statement
    HParser* create_stmt = h_sequence(create, whitespace, table, whitespace, 
                                    identifier, whitespace,
                                    h_ch('('), whitespace,
                                    column_list, whitespace,
                                    h_ch(')'), whitespace,
                                    h_ch(';'), NULL);
    
    // INSERT statement
    HParser* insert_stmt = h_sequence(insert, whitespace, into, whitespace,
                                    identifier, whitespace,
                                    h_ch('('), whitespace,
                                    h_sepBy(identifier, h_sequence(h_ch(','), whitespace, NULL)), whitespace,
                                    h_ch(')'), whitespace,
                                    values, whitespace,
                                    h_ch('('), whitespace,
                                    value_list, whitespace,
                                    h_ch(')'), whitespace,
                                    h_ch(';'), NULL);
    
    // SELECT statement
    HParser* select_stmt = h_sequence(select, whitespace,
                                    h_choice(h_ch('*'), h_sepBy(identifier, h_sequence(h_ch(','), whitespace, NULL)), NULL), whitespace,
                                    from, whitespace,
                                    identifier, whitespace,
                                    h_optional(h_sequence(where, whitespace, condition, NULL)),
                                    h_ch(';'), NULL);
    
    // Complete SQL parser
    return h_choice(create_stmt, insert_stmt, select_stmt, NULL);
}

H_RULE(sql_parser, init_parser());

int main() {
    HParser* parser = init_parser();
    
    const char* input = "CREATE TABLE users (id INTEGER, name TEXT, age INTEGER);";
    HParseResult* result = h_parse(parser, (const uint8_t*)input, strlen(input));
    
    if(result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
    
    return 0;
}