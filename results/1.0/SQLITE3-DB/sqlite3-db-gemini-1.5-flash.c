#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

// Structure to represent a row in the database
typedef struct {
    int id;
    char name[100];
    int age;
} Person;


// Function to create the database and table
int createDatabase(const char* dbname) {
    sqlite3* db;
    char* errmsg = 0;
    int rc = sqlite3_open(dbname, &db);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    char* sql = "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER);";
    rc = sqlite3_exec(db, sql, 0, 0, &errmsg);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", errmsg);
        sqlite3_free(errmsg);
        sqlite3_close(db);
        return 1;
    }

    sqlite3_close(db);
    return 0;
}


// Function to insert data into the database
int insertData(const char* dbname, const Person* person) {
    sqlite3* db;
    char* errmsg = 0;
    char sql[255];
    int rc;

    rc = sqlite3_open(dbname, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    snprintf(sql, sizeof(sql), "INSERT INTO people (name, age) VALUES ('%s', %d);", person->name, person->age);
    rc = sqlite3_exec(db, sql, 0, 0, &errmsg);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", errmsg);
        sqlite3_free(errmsg);
        sqlite3_close(db);
        return 1;
    }
    sqlite3_close(db);
    return 0;
}


int main() {
    const char* dbname = "mydatabase.db";
    Person person1 = {1, "John Doe", 30};
    Person person2 = {2, "Jane Smith", 25};

    // Create the database and table
    if (createDatabase(dbname) != 0) return 1;

    // Insert data into the database
    if (insertData(dbname, &person1) != 0) return 1;
    if (insertData(dbname, &person2) != 0) return 1;


    printf("Data inserted successfully!\n");
    return 0;
}
