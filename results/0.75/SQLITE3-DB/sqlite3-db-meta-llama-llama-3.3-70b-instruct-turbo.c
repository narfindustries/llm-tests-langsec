#include <hammer/hammer.h>
#include <stdint.h>
#include <sqlite3.h>

int main() {
    sqlite3* db;
    char* errorMessage;

    // Open the SQLite database
    int rc = sqlite3_open("input.db", &db);
    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }

    // Create a SQL statement to execute
    const char* sql = "SELECT * FROM meta";

    // Prepare the SQL statement
    sqlite3_stmt* stmt;
    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    // Execute the SQL statement and store the results in a hammer object
    hammer_object_t* obj = hammer_object_new();
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        int num_cols = sqlite3_column_count(stmt);
        for (int i = 0; i < num_cols; i++) {
            const unsigned char* text = sqlite3_column_text(stmt, i);
            hammer_object_add_string(obj, text, strlen((const char*)text));
        }
    }

    // Finalize the SQL statement and close the database
    sqlite3_finalize(stmt);
    sqlite3_close(db);

    // Output the hammer object
    hammer_output(obj, "output_hammer");

    return 0;
}