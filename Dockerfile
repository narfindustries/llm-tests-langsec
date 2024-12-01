FROM ubuntu:24.04

RUN apt-get update && apt-get -y upgrade

RUN apt-get install -y build-essential

RUN apt-get install -y curl default-jre-headless

# Installing Kaitai Struct
RUN curl -LO https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb
RUN dpkg -i ./kaitai-struct-compiler_0.10_all.deb

RUN apt-get -y install python3 python3-pip

WORKDIR /ddl
COPY ksy_compiler.py /ddl