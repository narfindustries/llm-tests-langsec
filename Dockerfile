FROM debian:12-slim

RUN apt-get update
RUN apt-get -y install wget build-essential curl
RUN wget https://github.com/kaitai-io/kaitai_struct_compiler/releases/download/0.10/kaitai-struct-compiler_0.10_all.deb

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN apt-get -y install apt-transport-https
RUN apt-get update && apt-get install -y --no-install-recommends binutils bison ccache cmake file flex gcc g++ git google-perftools jq libfl-dev libgoogle-perftools-dev libkrb5-dev libmaxminddb-dev libpcap0.8-dev libssl-dev locales-all make ninja-build python3 python3-dev python3-pip python3-setuptools python3-wheel swig zlib1g-dev openjdk-17-jre-headless

RUN dpkg -i *.deb

RUN git clone --recursive https://github.com/zeek/spicy

RUN git clone https://github.com/GaloisInc/daedalus.git

RUN cd spicy && ./configure && cd build && make

ENV PATH="$PATH:/root/.ghcup/bin"

RUN apt-get -y install libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

RUN cd daedalus && cabal install exe:daedalus --installdir=DIR --overwrite-policy=always

RUN apt-get -y install scons pkg-config libglib2.0-dev

RUN git clone https://github.com/UpstandingHackers/hammer && cd hammer && scons && scons install
