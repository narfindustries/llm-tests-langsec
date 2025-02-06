import os
from typing import Dict, Any
import json
import sys
import threading
import time
import logging
import argparse
from anthropic.types import TextBlock

from compile import compile_file, get_current_dir
from db import Database

from LLMFormatGeneration import LLMFormatGeneration

# Configure logging
logging.basicConfig(
    level=logging.CRITICAL, format="%(asctime)s - %(levelname)s - %(message)s"
)

# Parse the file with the options
parsed_options = json.loads(open("options.json").read())


class DSLGenerator:

    def __init__(self, cur_time: int, dbname: str, table_name: str):
        self.cur_time = cur_time
        self.dbname = dbname
        self.table_name = table_name

    def set_current_dir_and_command(self, ddl, dir_path, filename):
        full_input_path = os.path.join(dir_path, filename)
        # Set the command variable to call the compiler
        current_dir = None
        cmd = None
        if ddl == "Kaitai Struct":
            current_dir = get_current_dir(dir_path, filename, "output_kaitai")
            cmd = [
                "kaitai-struct-compiler",  # Assumes kaitai-struct-compiler is in PATH
                "-t",
                "python",  # Target Python
                "--outdir",
                current_dir,  # Destination directory
                full_input_path,
            ]
        elif ddl == "Daedalus":
            current_dir = get_current_dir(dir_path, filename, "output_daedalus")
            cmd = [
                parsed_options["compiler_paths"][
                    ddl
                ],  # This might need to change to the correct path
                "compile-hs",
                full_input_path,
                "--out-dir",
                current_dir,  # Destination directory
            ]

        elif ddl == "Zeek Spicy":
            current_dir = get_current_dir(dir_path, filename, "output_spicy")
            cmd = [
                parsed_options["compiler_paths"][ddl],
                "-j",
                "-o",
                current_dir + "/tmp.hlto",
                full_input_path,
            ]
        elif ddl == "DFDL":
            current_dir = get_current_dir(dir_path, filename, "output_dfdl")
            cmd = [
                parsed_options["compiler_paths"][ddl],
                "generate",
                "c",
                "-s",
                full_input_path,
                current_dir,
            ]
        elif ddl == "Hammer":
            current_dir = get_current_dir(dir_path, filename, "output_hammer")
            cmd = ["gcc", full_input_path, "-o", current_dir + "/output", "-lhammer"]
        elif ddl == "Rust Nom":
            current_dir = get_current_dir(dir_path, filename, "output_nom")
            os.system(f"cp -r cargo_template/* {current_dir}")
            os.system(f"cp {full_input_path} {current_dir}/src/main.rs")
            cmd = ["/home/user/.cargo/bin/cargo", "check"]
        else:
            print("Compiler not found")
        return (current_dir, cmd, full_input_path)

    def send_messages(self, function, model, messages, query, format, ddl, filename):
        if len(messages) == 0:
            if "claude" not in model:
                messages = [
                    {
                        "role": "system",
                        "content": "You are a software developer who has read standards for several network protocols and file formats and knows the syntax of Data Description Languages like Kaitai Struct, DaeDalus, DFDL, and Zeek Spicy.",
                    }
                ]

        messages.append({"role": "user", "content": query})
        response = function(model, messages)

        if isinstance(response, str):
            # Likely an API error
            db = Database(self.dbname)
            error = {
                "timestamp": str(time.time()),
                "format": format,
                "llm": model,
                "ddl": ddl,
                "error": response,
            }
            db.insert_data_errors(self.table_name, error)
            self.delete_file_if_exists(filename)
            import sys

            sys.exit(1)
        elif isinstance(response, TextBlock):
            messages.append({"role": "assistant", "content": response.text})
        else:
            # It is an OpenAI object
            messages.append({"role": "assistant", "content": response.content})
        return messages

    def run_queries_per_model(
        self,
        dir_path: str,
        format: str,
        specification: str,
        ddl: str,
        output: str,
        extension: str,
        model: str,
        function,
    ):
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
        filename = f"{format.lower().replace(' ', '-')}-{model.lower().replace(' ', '-').replace('/', '-')}.{extension}"  # Step 1
        # Check if file already exists before proceeding
        if os.path.exists(f"{dir_path}/{filename}"):
            logging.info(f"File {filename} already exists, skipping generation")
            return
 
        if (
            ddl in ["Kaitai Struct", "Rust Nom", "Hammer"]
            and model == "gemini-1.5-flash"
        ):
            time.sleep(60)
        db = Database(self.dbname)
        # Specify the two queries needed
        messages = []
        query_1 = f"You are a software developer who has read the {specification} for the {format}. Can you list all the fields in the specification along with all the values each field can take?"
        messages = self.send_messages(function, model, messages, query_1, format, ddl, filename)
        print(messages)
        query_2 = f"Can you use this knowledge to generate a {ddl} specification for the {format} in {output} format? Make sure to cover the entire specification including any optional fields. Do not provide any text response other than the {format} specification. Show only the complete response. Do not wrap the response in any markdown."

        if ddl == "Hammer" or ddl == "Rust Nom":
            query_2 = f"Can you use this knowledge to generate a {output} program with {ddl} parser combinator bindings for the {format}? Include a main function that can take input in a binary file from a command line argument. Make sure to cover the entire specification including any optional fields. Do not provide any text response other than the code. Show only the complete response. Do not wrap the response in any markdown."

            if ddl == "Hammer":
                query_2 += "Make sure that the includes statement is <hammer/hammer.h>."

        messages = self.send_messages(function, model, messages, query_2, format, ddl, filename)
        response = messages[-1]["content"]

        self.create_response_file(response, format, dir_path, filename)
        current_response = response

        (current_dir, cmd, full_input_path) = self.set_current_dir_and_command(
            ddl, dir_path, filename
        )

        compilation_output = compile_file(cmd, current_dir)  # Step 4

        print(model, compilation_output, 0)
        logging.info(compilation_output)

        self.insert_data_into_db(
            db, compilation_output, model, ddl, format, 0, current_response
        )

        # If the compilation failed, then we need to ask the LLM to fix the specification
        counter = parsed_options["tries"]  # Set this value in options.json
        while not compilation_output["success"] and counter != 0:
            logging.info(current_response)
            counter = counter - 1
            message = compilation_output["message"].replace("\n", " ")
            query_3 = f'The previous response gave me an error. Can you use this error message: "{message}" to improve the specification and give me an improved, complete, and fixed {ddl} specification in {output} format. Give me only the complete generated code and no text with it. Ensure that the previous requirements are still met.'

            messages = self.send_messages(
                function, model, messages, query_3, format, ddl, filename
            )
            response3 = messages[-1]["content"]

            self.create_response_file(response3, format, dir_path, filename)
            if ddl == "Rust Nom":
                os.system(f"cp {full_input_path} {current_dir}/src/main.rs")

            current_response = response3

            # Overwrite the compilation_output variable
            compilation_output = compile_file(cmd, current_dir)
            print(model, compilation_output, parsed_options["tries"] - counter)
            logging.info(compilation_output)
            self.insert_data_into_db(
                db,
                compilation_output,
                model,
                ddl,
                format,
                parsed_options["tries"] - counter,
                current_response,
            )
        if model == "gemini-1.5-flash":
            time.sleep(30)

    def generate_specifications_per_format(
        self,
        dir_path: str,
        format: str,
        specification: str,
        ddls: list,
        temperature: float,
    ):
        """
        For each model, run the queries and generate the specifications.
        This function invokes different threads to ensure some parallelism
        """
        comparison = LLMFormatGeneration(temperature)
        threads = []

        for model, function in comparison.llms.items():
            for ddl in ddls:
                # The format names contain spaces, so change them all to hyphens
                t = threading.Thread(
                    target=self.run_queries_per_model,
                    args=(
                        dir_path,
                        format,
                        specification,
                        ddl,
                        ddls[ddl][0],
                        ddls[ddl][1],
                        model,
                        function,
                    ),
                )
                threads.append(t)
                t.start()

        # Wait for all the threads to terminate
        for t in threads:
            t.join(300)

    def insert_data_into_db(
        self,
        db: Database,
        compilation_output: Dict[str, Any],
        model: str,
        ddl: str,
        format: str,
        try_no: int,
        current_response: str,
    ):
        """Insert the data into the database after the compilation is complete"""
        # Assume first that the compilation was not successful and set the value to False
        success = "False"
        if compilation_output["success"]:
            success = "True"
        db.insert_data(
            self.table_name,
            {
                "timestamp": str(time.time()),
                "llm": model,
                "ddl": ddl,
                "format": format,
                "compiled": success,
                "try": try_no,
                "output_response": current_response,
            },
        )

    def create_response_file(
        self, response: str, format: str, dir_path: str, filename: str
    ):
        """
        Create a file with the response from the LLM
        """
        file_desc = open(f"{dir_path}/{filename}", "w")
        # Some LLMs give you markdown encapsulated code
        # We are removing the lines that start with these characters
        lines = response.split("\n")
        cleaned_lines = [
            line
            for line in lines
            if not line.strip().startswith("```")
            and not line.strip().startswith("---")
            and not line.strip().startswith("...")
        ]
        response = "\n".join(cleaned_lines)
        file_desc.write(response)
        file_desc.close()
        # Write the output into the corresponding files

    def delete_file_if_exists(self, filepath: str):
        """Delete a file if it exists at the given path"""
        if os.path.exists(filepath):
            os.remove(filepath)

def main():

    # Initialize the argument parser
    parser = argparse.ArgumentParser(
        description="A CLI tool with --format and --time options."
    )

    # Add the --format flag
    parser.add_argument(
        "--format",
        choices=["file", "network", "all", "test"],
        help="select 'file' for file formats or 'network' for network protocols",
        default="all",  # Provide a default format
    )

    # Add the --time flag
    parser.add_argument(
        "--time",
        type=str,
        help="Specify a timestamp if you want to continue a previous execution",
        default=None,
    )
    # Add the --skip-check flag
    parser.add_argument(
        "--skip-check",
        action="store_true",
        help="Skip checking whether all API keys are present if set",
        default=False,
    )

    args = parser.parse_args()

    # Add list of API keys to check
    required_keys = [
        "OPENAI_API_KEY",
        "ANTHROPIC_API_KEY",
        "GOOGLE_API_KEY",
        "TOGETHER_API_KEY",
        "DEEPSEEK_API_KEY",
    ]

    if not args.skip_check:
        # Check if all required API keys are present
        missing_keys = [key for key in required_keys if not os.getenv(key)]
        if missing_keys:
            print(f"Error: Missing required API keys: {', '.join(missing_keys)}")
            print(
                "Please set the environment variables or use --skip-check to bypass this check"
            )
            sys.exit(1)

    db = Database("test.db")
    cur_time = int(time.time())
    if args.time:
        cur_time = int(args.time)

    temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
    # Combining parsing the file-formats and network protocols in one loop to make things easier
    specs = None
    ddls = parsed_options["DDLs"]
    if args.format == "all":
        specs = (
            parsed_options["file-formats"].items()
            | parsed_options["network-protocols"].items()
        )
    elif args.format == "file":
        specs = parsed_options["file-formats"].items()
    elif args.format == "network":
        specs = parsed_options["network-protocols"].items()
    elif args.format == "test":
        specs = parsed_options["test"].items()
        temperatures = [0.5]  # Simplifying the test
        ddls = {"Rust Nom": ["Rust", "rs"]}
    # ddls = {"Zeek Spicy": ["zeek spicy", "spicy"]}

    for temp in temperatures:
        table_name = f"t_{str(temp).replace('.','_')}_{str(cur_time)}"
        generator = DSLGenerator(cur_time, "test.db", table_name)
        db.create_table(table_name)  # Table name cannot start with an integer
        db.create_errors_table(table_name)
        for format, spec in specs:
            dir_path = f"generated/{cur_time}/{temp}/{format.replace(' ', '-')}"  # Having spaces here seems to really mess things up
            if not os.path.exists(dir_path):
                os.makedirs(dir_path)
            # The last three arguments would actually come from the parsed_options["DDLs"] field
            # for ddl in ddls: # This is a dictionary, so we need to iterate over the keys
            generator.generate_specifications_per_format(
                dir_path, format, spec, ddls, temp
            )


if __name__ == "__main__":
    main()
