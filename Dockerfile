FROM node:4

MAINTAINER Willem van den Ende

ENV PURESCRIPT_DOWNLOAD_SHA1 87de6ef5d9cf7eee059a9be6c61b5711abf121df

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.11.5/linux64.tar.gz \
    && echo "$PURESCRIPT_DOWNLOAD_SHA1 linux64.tar.gz" | sha1sum -c - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PSC_PACKAGE_DOWNLOAD_SHA1 9cbf72e45569320d0ca3ee8cfbe585a42bb7049c

RUN cd /opt \
    && wget https://github.com/purescript/psc-package/releases/download/v0.1.4/linux64.tar.gz \
    && echo "$PSC_PACKAGE_DOWNLOAD_SHA1 linux64.tar.gz" | sha1sum -c - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

RUN userdel node
RUN useradd -m -s /bin/bash pureuser

WORKDIR /home/pureuser

USER pureuser

RUN mkdir tmp && cd tmp 

ADD psc-package.json .

RUN /opt/psc-package/psc-package update

COPY src src/

RUN /opt/psc-package/psc-package build

