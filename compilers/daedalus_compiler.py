import os
import subprocess
import json

def compile_daedalus_file(directory, daedalus_file):
    """
    Find and compile DaeDaLus Spec (.ddl) files to HS.
    
    :param directory: Path to the directory containing .ddl files
    """
    # Ensure the directory exists
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory.")
        return

    # Create a directory for compiled C++ files
    cpp_output_dir = os.path.join(directory, 'daedalus_output')
    os.makedirs(cpp_output_dir, exist_ok=True)

    # Compile each .ddl file
    full_ddl_path = os.path.join(directory, daedalus_file)

    current_dir = os.path.join(cpp_output_dir, daedalus_file.split('.')[0])
    os.makedirs(current_dir, exist_ok=True)

    # TODO: Do we need to compile if the compiled file already exists?
    # This is the old ksy code, but in DDL we use haskell.
    if any(file.endswith('.py') for file in os.listdir(current_dir)):
        print(f"Directory {current_dir} contains a .py file. Skipping compilation.")
        return

    try:
        options = json.loads(open('options.json').read())
        # Construct the DaeDaLus command
        cmd = [
            options["compiler_paths"]["Daedalus"], # This might need to change to the correct path 
            'compile-hs',
            full_ddl_path,
            '--out-dir', current_dir,      # Destination directory
        ]
        
        # Run the compilation process
        result = subprocess.run(
            cmd, 
            capture_output=True, 
            text=True, 
            check=True
        )
        
        # Successful compilation
        return {
            'success': True,
            'message': 'Compilation successful',
            'output_files': os.listdir(current_dir)
        }

    except subprocess.CalledProcessError as e:
        # Compilation failed
        return {
            'success': False,
            'message': f"Compilation error:\nSTDERR: {e.stderr}",
            'error_code': e.returncode
        }
    
    except FileNotFoundError:
        # DaeDalus compiler not installed
        return {
            'success': False,
            'message': "DaeDalus not found. Please install it and ensure it's in your system PATH."
        }
    
    except Exception as e:
        # Catch any other unexpected errors
        return {
            'success': False,
            'message': f"Unexpected error during compilation: {str(e)}"
        }

