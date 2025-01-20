# How Effective are LLMs at Generating Accurate Data Descriptions?

- Install necessary dependencies (pip packages for the LLM libraries)
- Install the runtime libraries for the DSLs

    - Install [Kaitai Struct's compiler](https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb)
    - Download [Apache Daffodil binaries](https://www.apache.org/dyn/closer.lua/download/daffodil/3.9.0/bin/apache-daffodil-3.9.0-bin.zip)
    - Build the Spicy tools from Source: [Spicy Documentation](https://docs.zeek.org/projects/spicy/en/latest/installation.html#id15)
    - Build Galois' DaeDaLus tooling from source: [DaeDaLus](https://github.com/GaloisInc/daedalus)

- Add API keys as environment variables

```
export DEEPSEEK_API_KEY=""
export GOOGLE_API_KEY=""
export ANTHROPIC_API_KEY=""
export OPENAI_API_KEY=""
export XAI_API_KEY=""
export FIREWORKS_AI_KEY=""
```

- Run the script to generate the DSLs using LLM queries
`python3 create_dsls.py`

- Test the generated library code by running a corpus of files through them
`python3 corpora-test.py`

## DSLs supported:
- [Kaitai Struct](https://kaitai.io/)
- [DaeDaLus](https://github.com/GaloisInc/daedalus)
- [DFDL]()
- [Zeek Spicy]()
- [Hammer]()
- [Rust Nom]()
## Planned support
- Parsley
- EverParse

# TODO
[ ] Prepare a `requirements.txt` file