#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NAME_LENGTH 100
#define MAX_RECORDS 1000

typedef struct {
    int id;
    char name[MAX_NAME_LENGTH];
    double salary;
} Employee;

static HParser* employee_parser(void);
static HParseResult* employee_action(void* parsed, void* user_data);
static int sqlite_callback(void* data, int argc, char** argv, char** col_names);
static int save_to_sqlite(Employee* employees, int num_employees);
static int read_from_sqlite(Employee* employees);

static HParser* employee_parser() {
    HParser* id_parser = h_int(10);
    HParser* name_parser = h_many1(h_ch_range('a', 'z'));
    HParser* salary_parser = h_double();

    HParser* employee_struct = h_sequence(
        id_parser,
        h_whitespace(h_ch(' ')),
        h_whitespace(name_parser),
        h_whitespace(h_ch(' ')),
        salary_parser,
        NULL
    );

    return h_action(employee_struct, employee_action, NULL);
}

static HParseResult* employee_action(void* parsed, void* user_data) {
    HArrayList* list = (HArrayList*)parsed;
    Employee* emp = malloc(sizeof(Employee));

    emp->id = *(int*)h_arr_get(list, 0);
    strncpy(emp->name, h_arr_get(list, 2), MAX_NAME_LENGTH - 1);
    emp->salary = *(double*)h_arr_get(list, 4);

    return h_make_result(NULL, emp);
}

static int sqlite_callback(void* data, int argc, char** argv, char** col_names) {
    Employee* employees = (Employee*)data;
    static int count = 0;

    if (count < MAX_RECORDS) {
        employees[count].id = atoi(argv[0]);
        strncpy(employees[count].name, argv[1], MAX_NAME_LENGTH - 1);
        employees[count].salary = atof(argv[2]);
        count++;
    }

    return 0;
}

static int save_to_sqlite(Employee* employees, int num_employees) {
    sqlite3* db;
    char* err_msg = 0;
    int rc = sqlite3_open("employees.db", &db);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return rc;
    }

    const char* create_table = 
        "CREATE TABLE IF NOT EXISTS employees ("
        "id INTEGER PRIMARY KEY, "
        "name TEXT, "
        "salary REAL);";

    rc = sqlite3_exec(db, create_table, 0, 0, &err_msg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        sqlite3_close(db);
        return rc;
    }

    for (int i = 0; i < num_employees; i++) {
        char sql[256];
        snprintf(sql, sizeof(sql), 
            "INSERT INTO employees (id, name, salary) VALUES (%d, '%s', %f);", 
            employees[i].id, employees[i].name, employees[i].salary);

        rc = sqlite3_exec(db, sql, 0, 0, &err_msg);
        if (rc != SQLITE_OK) {
            fprintf(stderr, "SQL error: %s\n", err_msg);
            sqlite3_free(err_msg);
            sqlite3_close(db);
            return rc;
        }
    }

    sqlite3_close(db);
    return SQLITE_OK;
}

static int read_from_sqlite(Employee* employees) {
    sqlite3* db;
    char* err_msg = 0;
    int rc = sqlite3_open("employees.db", &db);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return rc;
    }

    const char* sql = "SELECT * FROM employees;";
    rc = sqlite3_exec(db, sql, sqlite_callback, employees, &err_msg);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        sqlite3_close(db);
        return rc;
    }

    sqlite3_close(db);
    return SQLITE_OK;
}

int main() {
    HParser* parser = employee_parser();
    Employee employees[MAX_RECORDS];
    int num_employees = 0;

    char input[] = "1 johndoe 50000.50\n2 janedoe 60000.75\n3 bobsmith 55000.25";
    HParseResult* result = h_parse(parser, (uint8_t*)input, strlen(input));

    if (result && result->ast) {
        HArrayList* parsed_list = (HArrayList*)result->ast;
        for (size_t i = 0; i < h_arraylist_length(parsed_list); i++) {
            Employee* emp = h_arraylist_get(parsed_list, i);
            employees[num_employees++] = *emp;
        }

        if (save_to_sqlite(employees, num_employees) == SQLITE_OK) {
            memset(employees, 0, sizeof(employees));
            read_from_sqlite(employees);

            for (int i = 0; i < num_employees; i++) {
                printf("ID: %d, Name: %s, Salary: %.2f\n", 
                    employees[i].id, employees[i].name, employees[i].salary);
            }
        }
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    return 0;
}