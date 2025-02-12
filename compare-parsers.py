import os
import subprocess
from pathlib import Path
import sys
from kaitaistruct import KaitaiStream
from scapy.all import *
from scapy.contrib.modbus import ModbusADUResponse, ModbusADURequest
from PIL import Image


def run_pillow(filename: str):
    """
    This should be able to run any image file
    """
    im = Image.open(filename)
    im.verify()


def run_rust_project(path: str, filename) -> int:
    """
    path: the location of the project to run. Cargo requires you to run a command from that location.
    filename: The fully resolved path to the input file to the Rust Nom parser

    Returns: 1 if compilation failed, 2 if input parsing failed, 0 if parsing succeeded
    """
    result = subprocess.run(
        ["/home/user/.cargo/bin/cargo", "check"], cwd=path, capture_output=True
    )
    if result.returncode != 0:
        return 1
    result = subprocess.run(
        ["/home/user/.cargo/bin/cargo", "run", "--", filename],
        cwd=path,
        capture_output=True,
    )

    if result.returncode == 0:
        return 0
    else:
        return 2


def run_hammer_project(path: str, filename) -> int:
    """
    path: the location of the project to run. Cargo requires you to run a command from that location.
    filename: The fully resolved path to the input file to the Rust Nom parser

    Returns: 1 if compilation failed, 2 if input parsing failed, 0 if parsing succeeded
    """
    try:
        result = subprocess.run(
            ["./output", filename], cwd=path, capture_output=True, timeout=300
        )
    except FileNotFoundError as e:
        # print(e)
        return 1
    except subprocess.TimeoutExpired as e:
        # print(e)
        return 2

    if (
        result.returncode == 0
        and b"fail" not in result.stdout
        and b"Fail" not in result.stdout
    ):
        return 0
    # print(result.stdout)
    return 2


def run_kaitai_project(full_path: str, Jpeg):
    """Invoke the class on the input stream"""
    with open(full_path, "rb") as f:
        stream = KaitaiStream(f)
        try:
            parsed = Jpeg(stream)
            return 0
        except Exception as e:
            # print(e)
            return -1


def run_spicy_project(parser_file, binary_file):
    """Validates a binary file against a Spicy parser."""

    if not os.path.exists(parser_file + "/tmp.hlto"):
        print(f"Parser file not found: {parser_file}")
        return 1
    file_read = open(binary_file, "rb").read()
    try:
        result = subprocess.run(
            ["../spicy/build/bin/spicy-driver", parser_file + "/tmp.hlto"],
            # capture_output=True, text=True, check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            input=file_read,
        )
        print("Validation successful:", result.stdout)
        return 0
    except subprocess.CalledProcessError as e:
        print("Validation failed:", e.stderr)
        return 2


def kaitai_comparison(timestamp: str, files, generated_folder: str, folder_name=None):
    """Compare Kaitai Implementations"""
    temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
    result_count = {t: {} for t in temperatures}
    if folder_name is None:
        folder_name = generated_folder
    for t in temperatures:
        kaitai_path = f"generated/{timestamp}/{t}/{folder_name}/output_kaitai/"
        folders = os.listdir(kaitai_path)
        sys.path.append(kaitai_path)
        for proj in folders:
            for filename in files:
                full_path = Path(filename).resolve()
                # print(full_path)
                if not result_count[t].get(proj):
                    result_count[t][proj] = 0
                result = -1
                try:
                    modulename = generated_folder.lower()
                    classname = modulename.capitalize()
                    module = __import__(f"{proj}.{modulename}", fromlist=[classname])
                    Jpeg = getattr(module, classname)
                except Exception:
                    # print(e)
                    result_count[t][proj] = -1
                    break
                result = run_kaitai_project(full_path, Jpeg)
                if result == 0:
                    result_count[t][proj] += 1
                else:
                    break
    return result_count


def rust_comparison(timestamp: str, files, generated_folder: str, folder_name=None):
    """Run the generated Rust implementations"""
    temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
    result_count = {t: {} for t in temperatures}
    for filename in files:
        full_path = Path(filename).resolve()
        # run_pillow(filename)
        if folder_name is None:
            folder_name = generated_folder
        for t in temperatures:
            nom_path = f"generated/{timestamp}/{t}/{folder_name}/output_nom/"
            folders = os.listdir(nom_path)
            for proj in folders:
                if not result_count[t].get(proj):
                    result_count[t][proj] = 0
                result = run_rust_project(nom_path + proj, full_path)
                if result == 0:
                    result_count[t][proj] += 1
                elif result == 1:
                    result_count[t][proj] = -1

    return result_count


def hammer_comparison(timestamp: str, files, generated_folder: str, folder_name: None):
    """Count the number of successfully running Hammer implementations"""
    temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
    result_count = {t: {} for t in temperatures}
    if folder_name is None:
        folder_name = generated_folder
    for t in temperatures:
        hammer_path = f"generated/{timestamp}/{t}/{folder_name}/output_hammer/"
        folders = os.listdir(hammer_path)
        for proj in folders:
            for filename in files:
                full_path = Path(filename).resolve()
                # print(filename, full_path)
                if not result_count[t].get(proj):
                    result_count[t][proj] = 0
                result = run_hammer_project(hammer_path + proj, full_path)
                # print(result, proj)
                if result == 0:
                    result_count[t][proj] += 1
                elif result == 1:
                    result_count[t][proj] = -1
    return result_count


def spicy_comparison(
    timestamp: str, format: str, generated_folder: str, folder_name: None
):
    """Run the compiled spicy files"""
    temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
    result_count = {t: {} for t in temperatures}
    folder_path = f"{input_files}/{format}"
    if folder_name is None:
        folder_name = generated_folder
    for t in temperatures:
        hammer_path = f"generated/{timestamp}/{t}/{folder_name}/output_spicy/"
        folders = os.listdir(hammer_path)
        for proj in folders:
            for f in os.listdir(folder_path):
                filename = f"{folder_path}/{f}"
                full_path = Path(filename).resolve()
                if not result_count[t].get(proj):
                    result_count[t][proj] = 0
                result = run_spicy_project(hammer_path + proj, full_path)
                # print(result, proj)
                if result == 0:
                    result_count[t][proj] += 1
                elif result == 1:
                    result_count[t][proj] = -1
    return result_count


def compare_format(
    files, directory: str, extension: str, format: str, folder_name=None
):
    """Given a format, run all the comparisons"""
    r = rust_comparison(directory, files, format, folder_name)
    print(r)
    r = kaitai_comparison(directory, files, format, folder_name)
    print(r)
    r = spicy_comparison(directory, extension, format, folder_name)
    print(r)
    r = hammer_comparison(directory, files, format, folder_name)
    print(r)


input_files = "mutated_files"

formats = ["jpg", "gif", "png", "modbus", "arp"]
format_counts = {format: [] for format in formats}
# Compute the number of invalid files per format
for format in formats[:3]:
    folder_path = f"{input_files}/{format}"
    for file in os.listdir(folder_path):
        try:
            run_pillow(f"{folder_path}/{file}")
        except:
            format_counts[format].append(f"{folder_path}/{file}")

folder_path = f"{input_files}/arp"
for file in os.listdir(folder_path):
    f = open(f"{folder_path}/{file}", "rb").read()
    try:
        a = ARP(f)
        if a.op not in [1, 2]:
            format_counts["arp"].append(f"{folder_path}/{file}")
    except:
        format_counts["arp"].append(f"{folder_path}/{file}")


folder_path = f"{input_files}/modbus"
for file in os.listdir(folder_path):
    f = open(f"{folder_path}/{file}", "rb").read()
    try:
        a = ModbusADURequest(f)
        a = ModbusADUResponse(f)
    except:
        format_counts["modbus"].append(f"{folder_path}/{file}")

for format, values in format_counts.items():
    print(format, len(values))

if __name__ == "__main__":
    compare_format(format_counts["jpg"], "888", "jpg", "JPEG")
    compare_format(format_counts["gif"], "888", "gif", "GIF")
    compare_format(format_counts["modbus"], "888", "modbus", "Modbus")
    compare_format(format_counts["png"], "888", "png", "PNG", "PNG-Image")
    compare_format(format_counts["arp"], "888", "arp", "ARP")
