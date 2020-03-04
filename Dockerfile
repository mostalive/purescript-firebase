FROM node:13

RUN userdel node
RUN useradd -m -s /bin/bash pureuser

WORKDIR /home/pureuser
RUN apt update && \
    apt -y install default-jdk-headless
RUN npm install -g bower pulp firebase-tools
USER pureuser

ADD package.json .
ADD bower.json .
RUN npm install
RUN bower install
ADD travis-build.sh .
COPY src src
COPY test test
RUN firebase emulators:exec --only database ./travis-build.sh
