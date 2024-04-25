FROM fedora:latest

RUN dnf update -y && \
	dnf install -y clang clang-devel cmake libcxx libcxx-devel

COPY . /cawk

RUN cd cawk/scripts && awk -f main.awk ../include/cawk.h
RUN cd cawk/src && clang++ -std=gnu++2c -stdlib=libc++ -I ../include -o cawk cawk.cc && mv cawk /usr/bin
RUN rm -rf cawk

CMD bash
