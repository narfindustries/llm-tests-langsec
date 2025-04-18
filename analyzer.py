import os
import json
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib
import difflib

from db import Database
from LLMFormatGeneration import LLMFormatGeneration

class Analyzer:
    """Analysis class that creates all the necessary output files and figures"""
    def __init__(self, dbname: str) -> None:
        self.dbname = dbname
        self.db = Database(dbname)

    def extract_rq1_overall_table(self, formats, ddls, llms, cur_time) -> None:
        """
        This will extract the entire table for one temperature setting.
        We need to post process this data to extract True or False per format, ddl, and model.
        """
        print(llms)
        temperatures = ["0_0", "0_25", "0_5", "0_75", "1_0"]
        colors = [
            sns.light_palette("seagreen"),
            sns.light_palette("skyblue"),
            sns.light_palette("coral"),
            sns.light_palette("lavender"),
            sns.light_palette("gold"),
            sns.light_palette("teal"),
            sns.light_palette("teal"),
        ]
        for index, ddl in enumerate(ddls):
            per_ddl_list = []
            for form in formats:
                count_list = []
                for llm in llms:
                    count = 0
                    for temperature in temperatures:
                        tablename = (
                            f"t_{str(temperature).replace('.', '_')}_{str(cur_time)}"
                        )
                        # This only returns a list of successful compiles per format, ddl, and model.
                        result = self.db.get_compile_data(tablename, llm, ddl, form)
                        if len(result) > 0:
                            count += 1
                    count_list.append(count)
                per_ddl_list.append(count_list)
            # print(per_ddl_list)
            self.generate_heatmap(np.array(per_ddl_list), ddl, colors[index])
            # count_list.append(f"\\cellcolor{{{count}}} {count}")
            # print(options["lookup"][form] + "& " + " & ".join(count_list), end='')
            # print("\\\\")
        # print("\\hline")

    def generate_heatmap(self, data, ddl: str, color):
        """Create the heatmap for each DDL"""
        # Create a custom green colormap
        cmap = color

        # Add labels and title
        row_labels = [
            "PNG",
            "JPEG",
            "GIF",
            "TIFF",
            "DICOM",
            "NITF",
            "ELF",
            "ZIP",
            "GZIP",
            "SQLITE3",
            "NTP",
            "Bitcoin",
            "Modbus",
            "ARP",
            "MQTT",
            "HTTP/1.1",
            "TLS Hello",
            "ICMP",
            "DNS",
            "HL7 v2",
        ]
        column_labels = ["$G$", "$G_4$", "$G_O$", "$C_S$", "$C_H$", "$D$", "$L$"]
        # Plot the heatmap
        plt.rcParams["font.family"] = "Times New Roman"
        plt.figure(figsize=(3, 8))
        plt.tick_params(
            axis="x",
            which="both",
            bottom=False,
            top=True,
            labeltop=True,
            labelbottom=False,
        )
        sns.heatmap(
            data,
            cmap=cmap,
            annot=True,
            fmt="d",
            vmin=0,
            vmax=5,
            linewidths=0.5,
            linecolor="black",
            xticklabels=column_labels,
            yticklabels=row_labels,
            cbar=False,
        )
        plt.xticks(ha="center")
        plt.title(ddl)

        filename = f"figs/heatmaps/{ddl}.png"
        plt.savefig(filename, dpi=300, bbox_inches="tight")
        print(f"Heatmap saved to '{filename}'")

    def generate_line_graph(self, llms, ddl: str, format: str) -> None:
        """
        # Formats:
        # ARP for Kaitai Struct
        # ARP for Rust Nom
        X Axis is going to be Temperature Values
        Y Axis is Lines of Code
        One line each per LLM model
        """
        llm_data = self.db.get_lines_of_code(llms, ddl, format)
        print(llm_data)
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        markers = ["o", "s", "D", "^", "v", "p", "*"]

        # Plotting the data
        plt.figure(figsize=(10, 6))
        plt.rcParams["font.family"] = "Times New Roman"

        for index, (llm, values) in enumerate(llm_data.items()):
            plt.plot(temperatures, values, marker=markers[index], label=llm)

        # Chart formatting
        plt.title(f"ARP Parsers Generated in {ddl}", fontsize=16)
        plt.xlabel("Temperature", fontsize=14)
        plt.ylabel("Lines of Code", fontsize=14)
        plt.xticks(temperatures)
        plt.legend(title="LLMs", fontsize=12)
        plt.grid(True, linestyle="--", alpha=0.6)

        # Display the chart
        plt.tight_layout()
        plt.savefig(f"figs/rq-1-{ddl}.png", dpi=300, bbox_inches="tight")

    def generate_bar_chart_for_temperatures(self, llms):
        """
        This will generate a bar chart for each LLM model showing the number of successful compiles per temperature.
        """
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        total_array = []
        for llm in llms:
            total_compiled = []
            for temperature in temperatures:
                tablename = f"t_{str(temperature).replace('.', '_')}_888"
                list_of_compiled = set(
                    self.db.get_number_of_compiled(tablename, llm)
                )  # Remove duplicates
                percentage = (len(list_of_compiled) / (20 * 7)) * 100
                total_compiled.append(percentage)
            total_array.append(total_compiled)
        print(total_array)

        # Setting up the bar chart
        bar_width = 0.15
        x = np.arange(len(llms))
        data = np.array(total_array)

        font_style = {"font.family": "Times New Roman"}

        with matplotlib.pyplot.style.context(font_style):
            fig, ax = plt.subplots(figsize=(8, 8))

            colors = plt.cm.viridis(np.linspace(0, 1, len(temperatures)))
            plt.rcParams["font.family"] = "Times New Roman"

            for i, (temp, color) in enumerate(zip(temperatures, colors)):
                bars = ax.bar(
                    x + i * bar_width,
                    data[:, i],
                    width=bar_width,
                    label=temp,
                    color=color,
                )
                ax.bar_label(bars, fmt="%.0f", padding=3, fontsize=10)

            # Chart customization
            ax.set_xlabel("LLMs", fontsize=14)
            ax.set_ylabel("Compilation Percentage (%)", fontsize=14)
            ax.set_title(
                "Compilation Percentage of LLMs at Different Temperature Settings",
                fontsize=16,
            )
            ax.set_xticks(x + bar_width * 2)

            print(llms)
            labels = ["$G$", "$G_4$", "$G_O$", "$C_S$", "$C_H$", "$D$", "$L$"]
            ax.set_xticklabels(labels, fontsize=12)
            ax.legend(title="Temperature", fontsize=10)

            plt.tight_layout()
            plt.savefig("figs/bar.png", dpi=300, bbox_inches="tight")

    def generate_table_number_tries(self, llms):
        """
        Just going to do this for Hammer and Kaitai Struct
        Total & Try 0 & Try 1 & Try 2 & Try 3
        """
        llm_labels = ["$G$", "$G_4$", "$G_O$", "$C_S$", "$C_H$", "$D$", "$L$"]
        for index, llm in enumerate(llms):
            table = self.db.measure_num_tries("Kaitai Struct", llm, "888")
            print(
                f"{llm_labels[index]} & {len(table['total'])} & {len(table[0])} & {len(table[1])} & {len(table[2])} & {len(table[3])} & ",
                end="",
            )
            table = self.db.measure_num_tries("Hammer", llm, "888")
            print(
                f"{len(table['total'])} & {len(table[0])} & {len(table[1])} & {len(table[2])} & {len(table[3])} \\\\"
            )
            table = self.db.measure_num_tries("Zeek Spicy", llm, "888")
            print(
                f"{llm_labels[index]} & {len(table['total'])} & {len(table[0])} & {len(table[1])} & {len(table[2])} & {len(table[3])} & ",
                end="",
            )
            table = self.db.measure_num_tries("Rust Nom", llm, "888")
            print(
                f"{len(table['total'])} & {len(table[0])} & {len(table[1])} & {len(table[2])} & {len(table[3])} \\\\"
            )

    def generate_diff(self):
        """Compute differences between the two file inputs"""
        # Read the contents of both files
        [file1_lines, file2_lines] = self.db.compute_diff()

        # Generate the diff
        diff = difflib.unified_diff(file1_lines, file2_lines, lineterm="")

        # Format the diff output
        for line in diff:
            print(line)

    def generate_bar_chart_for_rq2(self):
        """
        This will generate a bar chart for each LLM model showing the number of successful compiles per temperature.
        """
        # Read and parse RQ3 data from JSON file
        with open("rq2.json", "r") as file:
            rq3_data = json.load(file)
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        total_array = []
        formats = ["JPEG", "GIF", "Modbus", "ARP"]
        totals = [
            len(os.listdir("sample_files/jpg")),
            len(os.listdir("sample_files/png")),
            len(os.listdir("sample_files/modbus")),
            len(os.listdir("sample_files/arp")),
        ]
        for iter, format in enumerate(formats):
            for ddl, value in rq3_data.items():
                total_array = []
                if ddl != "Zeek Spicy":
                    for llm in llms:
                        ddl_total = []
                        for _temperature, rates in value[format].items():
                            key = f"{format.lower()}-{llm.replace('/','-').lower().split('.')[0]}"
                            ddl_total.append((rates[key] / totals[iter]) * 100)

                        total_array.append(ddl_total)
                else:
                    continue
                print(total_array)

                # Setting up the bar chart
                bar_width = 0.15
                x = np.arange(len(llms))
                data = np.array(total_array)

                font_style = {"font.family": "Times New Roman"}

                with matplotlib.pyplot.style.context(font_style):
                    fig, ax = plt.subplots(figsize=(8, 8))

                    colors = plt.cm.viridis(np.linspace(0, 1, len(temperatures)))
                    plt.rcParams["font.family"] = "Times New Roman"

                    for i, (temp, color) in enumerate(zip(temperatures, colors)):
                        bars = ax.bar(
                            x + i * bar_width,
                            data[:, i],
                            width=bar_width,
                            label=temp,
                            color=color,
                        )
                        ax.bar_label(bars, fmt="%.0f", padding=3, fontsize=8)

                    # Chart customization
                    ax.set_xlabel("LLMs", fontsize=14)
                    ax.set_ylabel("Compilation Percentage (%)", fontsize=14)
                    ax.set_title(
                        f"Percentage of Accepted Files for {ddl} {format} Parser",
                        fontsize=16,
                    )
                    ax.set_xticks(x + bar_width * 2)

                    labels = ["$G$", "$G_4$", "$G_O$", "$C_S$", "$C_H$", "$D$", "$L$"]
                    ax.set_xticklabels(labels, fontsize=12)
                    ax.legend(title="Temperature", fontsize=10)

                    plt.tight_layout()
                    plt.savefig(
                        f"figs/rq2/{format}-{ddl}.png", dpi=300, bbox_inches="tight"
                    )

    def generate_latex_table_for_rq2(self):
        """
        This will generate a latex table showing the number of successful runs per temperature.
        """
        # Read and parse RQ3 data from JSON file
        with open("figs/rq2.json", "r") as file:
            rq3_data = json.load(file)
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        ddls = ["Kaitai Struct", "Hammer", "Rust Nom"]
        formats = ["JPEG", "GIF", "Modbus", "ARP"]
        totals = [
            len(os.listdir("sample_files/jpg")),
            len(os.listdir("sample_files/gif")),
            len(os.listdir("sample_files/modbus")),
            len(os.listdir("sample_files/arp")),
        ]
        temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
        for iter, format in enumerate(formats):
            for temperature in temperatures:
                # for temperature, rates in value[format].items():
                if temperature == "0.5":
                    print(f"{format} & \\textbf{{{temperature}}}", end="")
                else:
                    print(f" & \\textbf{{{temperature}}}", end="")
                for ddl in ddls:
                    value = rq3_data[ddl]
                    if ddl != "Zeek Spicy":
                        rates = value[format][temperature]
                        for llm in llms:
                            key = f"{format.lower()}-{llm.replace('/','-').lower().split('.')[0]}"
                            if rates[key] != -1:
                                print(f" & {(rates[key]/totals[iter])*100:.2f}", end="")
                            else:
                                print(" & - ", end="")
                    else:
                        continue
                print("\\\\")
            print("\\hline")

    def generate_latex_table_for_rq3(self):
        """
        This will generate a latex table showing the number of successful runs per temperature.
        """
        # Read and parse RQ3 data from JSON file
        with open("figs/rq3.json", "r") as file:
            rq3_data = json.load(file)
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        formats = ["JPEG", "GIF", "Modbus", "ARP"]
        ddls = ["Kaitai Struct", "Hammer", "Rust Nom"]
        totals = [318, 241, 107, 599]
        temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
        for iter, format in enumerate(formats):
            for temperature in temperatures:
                # for temperature, rates in value[format].items():
                if temperature == "0.5":
                    print(f"{format} & \\textbf{{{temperature}}}", end="")
                else:
                    print(f" & \\textbf{{{temperature}}}", end="")
                for ddl in ddls:
                    value = rq3_data[ddl]
                    if ddl != "Zeek Spicy":
                        rates = value[format][temperature]
                        for llm in llms:
                            key = f"{format.lower()}-{llm.replace('/','-').lower().split('.')[0]}"
                            if rates[key] != -1:
                                print(f" & {(rates[key]/totals[iter])*100:.0f}", end="")
                            else:
                                print(" & -", end="")
                    else:
                        continue
                print("\\\\")
            print("\\hline")
    
    def get_data_for_precision_recall(self):
        accepted_data = None
        rejected_data = None
        with open("figs/rq2.json", "r") as file:
            accepted_data = json.load(file)
        with open("figs/rq3.json", "r") as file:
            rejected_data = json.load(file)
        temperatures = [0.0, 0.25, 0.5, 0.75, 1.0]
        formats = ["JPEG", "GIF", "Modbus", "ARP"]
        ddls = ["Kaitai Struct", "Hammer", "Rust Nom"]
        # totals_negatives = [318, 241, 107, 599] # Not used for recall or precision
        totals_positives = [
            len(os.listdir("sample_files/jpg")),
            len(os.listdir("sample_files/gif")),
            len(os.listdir("sample_files/modbus")),
            len(os.listdir("sample_files/arp")),
        ]
        temperatures = ["0.0", "0.25", "0.5", "0.75", "1.0"]
        precision_string = ""
        recall_string = ""
        for iter, format in enumerate(formats):
            for temperature in temperatures:
                # for temperature, rates in value[format].items():
                if temperature == "0.5":
                    precision_string += f"{format} & \\textbf{{{temperature}}}"
                    recall_string += f"{format} & \\textbf{{{temperature}}}"
                else:
                    precision_string += f" & \\textbf{{{temperature}}}"
                    recall_string += f" & \\textbf{{{temperature}}}"
                for ddl in ddls:
                    if ddl != "Zeek Spicy":
                        accepted_rates = accepted_data[ddl][format][temperature]
                        rejected_rates = rejected_data[ddl][format][temperature]
                        for llm in llms:
                            key = f"{format.lower()}-{llm.replace('/','-').lower().split('.')[0]}"
                            if accepted_rates[key] != -1: # Denotes a successful compile
                                # Precision Calculation
                                # TP / (TP + FP)
                                # Denominator can be 0, so check for an exception
                                try:
                                    precision_string += f" & {(accepted_rates[key]/(accepted_rates[key] + rejected_rates[key])):.2f}"
                                except:
                                    precision_string += " & N/A"
                                # Recall Calculation
                                # TP / (TP + FN)
                                # False negatives = (Total wellformed files - Files among them rejected)
                                # Total number of positive samples is always going be > 0
                                recall_string += f" & {accepted_rates[key]/(totals_positives[iter]):.2f}"
                            else:
                                precision_string += " & -"
                                recall_string += " & -"
                    else:
                        continue
                precision_string += "\\\\\n"
                recall_string += "\\\\\n"
            precision_string += "\\hline\n"
            recall_string += "\\hline\n"
        print(precision_string)
        print("#"*50)
        print(recall_string)
    def calculate_precision_recall(self):
        self.get_data_for_precision_recall()

    def generate_smaller_heatmap(self, data, ddl: str, color):
        """Generate heatmaps for the smaller comparison in RQ4"""
        # Create a custom green colormap
        cmap = color

        # Add labels and title
        row_labels = [
            "JPEG",
            "Modbus",
        ]
        column_labels = ["$G$", "$G_4$", "$G_O$", "$C_S$", "$C_H$", "$D$", "$L$"]
        # Plot the heatmap
        plt.rcParams["font.family"] = "Times New Roman"
        plt.figure(figsize=(2, 1))
        plt.tick_params(
            axis="x",
            which="both",
            bottom=False,
            top=True,
            labeltop=True,
            labelbottom=False,
        )
        sns.heatmap(
            data,
            cmap=cmap,
            annot=True,
            fmt="d",
            vmin=0,
            vmax=5,
            linewidths=0.5,
            linecolor="black",
            xticklabels=column_labels,
            yticklabels=row_labels,
            cbar=False,
        )
        plt.xticks(ha="center")
        plt.title(ddl)

        filename = f"figs/rq4-{ddl}-before.png"
        plt.savefig(filename, dpi=300, bbox_inches="tight")
        print(f"Heatmap saved to '{filename}'")


dbname = "test.db"
options = json.loads(open("options.json").read())

llms = list(LLMFormatGeneration(0).llms.keys())
analyzer = Analyzer(dbname)
ddls = list(options["DDLs"].keys())

analyzer.extract_rq1_overall_table(options["file-formats"] | options["network-protocols"], ddls, llms, "888")
analyzer.generate_line_graph(llms, "Kaitai Struct", "ARP")
analyzer.generate_line_graph(llms, "Rust Nom", "ARP")
analyzer.generate_line_graph(llms, "Hammer", "ARP")
analyzer.generate_bar_chart_for_temperatures(llms)
analyzer.generate_table_number_tries(llms)
analyzer.generate_diff()
analyzer.generate_bar_chart_for_rq2()
analyzer.generate_latex_table_for_rq2()
analyzer.generate_latex_table_for_rq3()
analyzer.calculate_precision_recall()
