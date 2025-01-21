import os
import sys
from typing import Dict, Any
import json
import threading
import time
import logging
import argparse

from compile import compile_file, get_current_dir
from compilers.ksy_compiler import compile_ksy_file
from compilers.daedalus_compiler import compile_daedalus_file
from compilers.spicy_compiler import compile_spicy_file
from compilers.dfdl_compiler import compile_dfdl_file
from db import Database

from LLMFormatGeneration import LLMFormatGeneration

# Configure logging
logging.basicConfig(level=logging.CRITICAL, format='%(asctime)s - %(levelname)s - %(message)s')

# Parse the file with the options
parsed_options = json.loads(open("options.json").read())


class DSLGenerator:

    def __init__(self, cur_time: int, dbname: str):
        self.cur_time = cur_time
        self.dbname = dbname
        self.table_name = "t_"  + str(cur_time)

    def run_queries_per_model(self, dir_path: str, format: str, specification: str, ddl: str, output: str, extension: str, model: str, function):
        """
        This function gets executed in the threads

        Step 1: Create output file name: <format>-<model>.<extension>
        Step 2: Send query 1, wait for a response. 
        Step 3: Send query 2, get the response and store it in the file. This second response should be a file in the correct DSL syntax.
        Step 4: Compile the file using the compiler and create a database record.
        Step 5: If the compilation fails, send query 3, get the response and store it in the file. Create a database record for every response and compilation.
                This third response is stored in a file in the correct DSL syntax.
        Step 6: Step 5 is run again (max three times) until the compilation is successful.

        """
        db = Database(self.dbname)
        # Specify the two queries needed
        query_1 = f"You are a software developer who has read the {specification} for the {format}. Can you list all the fields in the specification along with all the values each field can take?"
        query_2 = f"Can you use this knowledge to generate a {ddl} specification for the {format} in {output} format? Make sure to cover the entire specification including any optional fields. Do not provide any text response other than the {format} specification. Show only the complete response. Do not wrap the response in any markdown."

        filename = f"{format.lower().replace(' ', '-')}-{model.lower().replace(' ', '-')}.{extension}" # Step 1
        current_response = None
        if not os.path.exists(f"{dir_path}/{filename}"):
            # print(format, filename)
            response1 = function(query_1, model) # Step 2
            # print(response1)
            response2 = function(query_2, model) # Step 3
            self.create_response_file(response2, format, dir_path, filename)
            current_response = response2

        # Check if the generated file can be compiled
        compilation_output = None
        full_input_path = os.path.join(dir_path, filename)
        cmd = None
        current_dir = None

        # Set the command variable to call the compiler
        if ddl == "Kaitai Struct":
            current_dir = get_current_dir(dir_path, filename, "output_kaitai")
            cmd = [
                'kaitai-struct-compiler',     # Assumes kaitai-struct-compiler is in PATH
                '-t', 'python',               # Target Python
                '--outdir', current_dir,         # Destination directory
                full_input_path
            ]
        elif ddl == "Daedalus":
            current_dir = get_current_dir(dir_path, filename, "output_daedalus")
            cmd = [
                parsed_options["compiler_paths"][ddl], # This might need to change to the correct path 
                'compile-hs',
                full_input_path,
                '--out-dir', current_dir,      # Destination directory
            ]
            
        elif ddl == "Zeek Spicy":
            current_dir = get_current_dir(dir_path, filename, "output_spicy")
            cmd = [
                parsed_options["compiler_paths"][ddl],
                '-j', '-o', current_dir + '/tmp.hlto',
                full_input_path
            ]
        elif ddl == "DFDL":
            current_dir = get_current_dir(dir_path, filename, "output_dfdl")
            cmd = [
                parsed_options["compiler_paths"][ddl],
                'generate', 'c', '-s', full_input_path, current_dir
            ]
        compilation_output = compile_file(cmd, current_dir) # Step 4

        print(compilation_output)
        logging.info(compilation_output)

        if compilation_output is None:
            # This means that there was already an output file in the folder, so we can skip the compilation
            return
        else:
            self.insert_data_into_db(db, compilation_output, model, ddl, format, current_response)

            # If the compilation failed, then we need to ask the LLM to fix the specification
            counter = parsed_options["tries"] # Set this value in options.json
            while not compilation_output["success"] and counter != 0:
                logging.info(current_response)
                counter = counter - 1
                message = compilation_output['message'].replace("\n", " ")
                query_3 = f"The previous response gave me an error. Can you use this error message: \"{message}\" to improve the specification and give me an improved, complete, and fixed {ddl} specification in {output} format. Give me only the complete generated code and no text with it."
                response3 = function(query_3, model)
                self.create_response_file(response3, format, dir_path, filename)
                current_response = response3

                # Overwrite the compilation_output variable
                compilation_output = compile_file(cmd, current_dir)
                self.insert_data_into_db(db, compilation_output, model, ddl, format, current_response)



    def generate_specifications_per_format(self, dir_path: str, format: str, specification: str, ddl: str, output: str, extension: str):
        """
        For each model, run the queries and generate the specifications.
        This function invokes different threads to ensure some parallelism
        """
        comparison = LLMFormatGeneration(parsed_options["temperature"])
        threads = []

        for model, function in comparison.llms.items():
            # The format names contain spaces, so change them all to hyphens
            t = threading.Thread(target = self.run_queries_per_model, args=(dir_path, format, specification, ddl, output, extension, model, function))
            threads.append(t)
            t.start()
        
        # Wait for all the threads to terminate
        for t in threads:
            t.join()

    def insert_data_into_db(self, db: Database, compilation_output: Dict[str, Any], model: str, ddl: str, format: str, current_response: str):
        """Insert the data into the database after the compilation is complete"""
        # Assume first that the compilation was not successful and set the value to False
        success = "False"
        if compilation_output["success"]:
            success = "True"
        db.insert_data(self.table_name, {
            "timestamp": str(time.time()),
            "llm": model,
            "ddl": ddl,
            "format": format,
            "compiled": success,
            "output_response": current_response
        })

    def create_response_file(self, response: str, format: str, dir_path: str, filename: str):
        """
        Check if there was an exception in the LLM command
        If not, then create a new file
        """
        if not "Error" in response: # Ensure that responses with errors do not get written
            file_desc = open(f"{dir_path}/{filename}", "w")
            # Some LLMs give you markdown encapsulated code
            # We are removing the lines that start with these characters
            lines = response.split("\n")
            cleaned_lines = [line for line in lines if not line.strip().startswith('```') and not line.strip().startswith('---') and not line.strip().startswith('...')]
            response = "\n".join(cleaned_lines)
            file_desc.write(response)
            file_desc.close()
            # Write the output into the corresponding files

def main():

    # Initialize the argument parser
    parser = argparse.ArgumentParser(description="A CLI tool with --format and --time options.")
    
    # Add the --format flag
    parser.add_argument(
        "--format",
        choices=["file", "network", "all"],
        help="select 'file' for file formats or 'network' for network protocols",
        default="all"  # Provide a default format
    )
    
    # Add the --time flag
    parser.add_argument(
        "--time",
        type=str,
        help="Specify a timestamp if you want to continue a previous execution",
        default=None
    )
    args = parser.parse_args()

    db = Database("test.db")
    cur_time = int(time.time())
    if args.time:
        cur_time = int(args.time)
    db.create_table("t_" + str(cur_time)) # Table name cannot start with an integer
    generator = DSLGenerator(cur_time, "test.db")
    # ddls = ["DFDL", "Zeek Spicy"]
    ddls = ["Kaitai Struct", "Daedalus"]

    # Combining parsing the file-formats and network protocols in one loop to make things easier
    specs = None
    if args.format == "all":
        specs = parsed_options["file-formats"].items() | parsed_options["network-protocols"].items()
    elif args.format == "file":
        specs = parsed_options["file-formats"].items()
    elif args.format == "network":
        specs = parsed_options["network-protocols"].items()
    
    for format, spec in specs:
        dir_path = f"generated/{cur_time}/{format.replace(' ', '-')}"
        if not os.path.exists(dir_path):
            os.makedirs(dir_path)
        # The last three arguments would actually come from the parsed_options["DDLs"] field
        for ddl in ddls:
            # generator.generate_specifications_per_format(dir_path, format, spec, "Kaitai struct", "yaml", "ksy")
            generator.generate_specifications_per_format(dir_path, format, spec, ddl,
                                                     parsed_options["DDLs"][ddl][0], 
                                                     parsed_options["DDLs"][ddl][1])

if __name__ == "__main__":
    main()