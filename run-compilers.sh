docker build -t ddl-code-generators .

docker run -v ./generated:/generated -it ddl-code-generators python3 ksy_compiler.py /generated/Network\ Time\ Protocol\ Version\ 4
docker run -v ./generated:/generated -it ddl-code-generators python3 ksy_compiler.py /generated/PNG\ Image

sudo chown -R user:user ./generated