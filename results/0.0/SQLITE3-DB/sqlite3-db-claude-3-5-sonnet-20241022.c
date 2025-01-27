#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_parser(void) {
    // Basic tokens
    HParser *whitespace = h_whitespace(h_ch(' '));
    HParser *newline = h_ch('\n');
    HParser *comma = h_ch(',');
    HParser *quote = h_ch('"');
    
    // Column types
    HParser *text_type = h_token("TEXT", 4);
    HParser *int_type = h_token("INTEGER", 7);
    HParser *real_type = h_token("REAL", 4);
    HParser *blob_type = h_token("BLOB", 4);
    HParser *type = h_choice(text_type, int_type, real_type, blob_type, NULL);
    
    // Column name - alphanumeric and underscore
    HParser *name_char = h_choice(h_ch('_'), h_alpha(), h_digit(), NULL);
    HParser *column_name = h_many1(name_char);
    
    // Column definition
    HParser *column = h_sequence(column_name, whitespace, type, NULL);
    
    // Multiple columns separated by commas
    HParser *columns = h_sepBy1(column, h_sequence(comma, whitespace, NULL));
    
    // Table name
    HParser *table_name = h_many1(name_char);
    
    // CREATE TABLE statement
    HParser *create = h_token("CREATE TABLE ", 13);
    HParser *statement = h_sequence(create, table_name, whitespace, 
                                  h_ch('('), whitespace,
                                  columns, whitespace,
                                  h_ch(')'), h_ch(';'),
                                  NULL);
    
    // INSERT statement
    HParser *insert = h_token("INSERT INTO ", 12);
    HParser *values = h_token("VALUES ", 7);
    
    // Value types
    HParser *number = h_many1(h_digit());
    HParser *string_content = h_many(h_not_in("\"", 1));
    HParser *string = h_sequence(quote, string_content, quote, NULL);
    HParser *value = h_choice(number, string, NULL);
    
    // Value list
    HParser *value_list = h_sepBy1(value, h_sequence(comma, whitespace, NULL));
    
    // Complete INSERT statement
    HParser *insert_stmt = h_sequence(insert, table_name, whitespace,
                                    values, h_ch('('), whitespace,
                                    value_list, whitespace,
                                    h_ch(')'), h_ch(';'),
                                    NULL);
    
    // SELECT statement
    HParser *select = h_token("SELECT ", 7);
    HParser *from = h_token("FROM ", 5);
    HParser *where = h_token("WHERE ", 6);
    
    // Column list for SELECT
    HParser *select_columns = h_choice(
        h_token("*", 1),
        h_sepBy1(column_name, h_sequence(comma, whitespace, NULL)),
        NULL
    );
    
    // WHERE clause components
    HParser *operator = h_choice(h_ch('='), h_ch('>'), h_ch('<'), NULL);
    HParser *condition = h_sequence(column_name, whitespace, 
                                  operator, whitespace,
                                  value, NULL);
    
    // Complete SELECT statement
    HParser *select_stmt = h_sequence(select, select_columns, whitespace,
                                    from, table_name,
                                    h_optional(h_sequence(whitespace, where, condition, NULL)),
                                    h_ch(';'),
                                    NULL);
    
    // Complete SQL parser
    return h_choice(statement, insert_stmt, select_stmt, NULL);
}

int main(int argc, char **argv) {
    HParser *parser = init_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}