# How Effective are LLMs at Generating Accurate Data Descriptions?

- Install necessary dependencies (pip packages for the LLM libraries)
- Add API keys as environment variables
- Run the script to generate the DSLs using LLM queries 
`python3 generate-dsls.py`

- Run the script to compile the DSLs into the corresponding environments
`./run-compilers.sh`

- Test the generated library code by running a corpus of files through them
`python3 corpora-test.py`

## DSLs supported:
- Kaitai Struct
## Planned support
- DFDL
- Daedalus
- Parsley
- EverParse

# TODO
[ ] Prepare a `requirements.txt` file