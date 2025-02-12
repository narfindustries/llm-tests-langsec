> **Disclaimer:** This is a research prototype. The code and data in this repository are provided as-is, without any warranty or guarantee of correctness. Use at your own risk.


# How Effective are LLMs at Generating Accurate Data Descriptions?

We tasks 7 LLMs with producing specifications for 20 data formats. `results/` contains the generated files---without any annotations on whether these files compile.

- Install necessary dependencies (pip packages for the LLM libraries)
- Install the runtime libraries for the DSLs

    - Install [Kaitai Struct's compiler](https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb)
    - Download [Apache Daffodil binaries](https://www.apache.org/dyn/closer.lua/download/daffodil/3.9.0/bin/apache-daffodil-3.9.0-bin.zip)
    - Build the Spicy tools from Source: [Spicy Documentation](https://docs.zeek.org/projects/spicy/en/latest/installation.html#id15)
    - Build Galois' DaeDaLus tooling from source: [DaeDaLus](https://github.com/GaloisInc/daedalus)
    - Clone the repository and build [Hammer](https://github.com/UpstandingHackers/hammer)
    ```bash
    git clone https://github.com/UpstandingHackers/hammer.git
    cd hammer
    scons
    sudo scons install
    ```

- Add API keys as environment variables.
    - [Together API](https://api.together.ai/) to invoke the Llama and Deepseek LLMs.
    - [OpenAI](https://platform.openai.com/docs/overview)
    - [Claude](https://console.anthropic.com/)
    - [Gemini](https://ai.google.dev/gemini-api/docs)

```
export GOOGLE_API_KEY=""
export ANTHROPIC_API_KEY=""
export OPENAI_API_KEY=""
export TOGETHER_API_KEY=""
```

## Components

Together AI and Gemini are compatible with OpenAI's Python library.

- `Dockerfile` builds a docker image with all of these compilers or executables installed.

- `options.json` contains the paths of the various DDL executables and the list of formats and their specification versions.

- `test.db` contains the final database with two entire runs, where the run labeled "888" was the most recent run.

- Run the script to generate the DSLs using LLM queries
`python3 create_dsls.py`

- To generate the set of figures and tables used in the paper
`python3 analyzer.py`

- Test the generated library code by running a corpus of files through them. This command needs a folder containing files per format.
`python3 compare-parsers.py`

## DSLs supported:
- [Kaitai Struct](https://kaitai.io/)
- [DaeDaLus](https://github.com/GaloisInc/daedalus)
- [DFDL](https://daffodil.apache.org/)
- [Zeek Spicy](https://docs.zeek.org/projects/spicy/en/latest/)
- [Hammer](https://github.com/UpstandingHackers/hammer)
- [Rust Nom](https://github.com/rust-bakery/nom)

# Acknowledgments

This work was supported in part by DOE NETL (DE-CR0000017) and 
the ARPA-H DIGIHEALS (Contract No. SP4701-23-C-0089). The views, opinions, and/or findings
expressed are those of the author(s) and should not be interpreted 
as representing the official views or policies of DOE, ARPA-H, or the U.S. Government.