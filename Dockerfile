FROM node:13


RUN userdel node
RUN useradd -m -s /bin/bash pureuser

WORKDIR /home/pureuser
RUN apt update && \
    apt -y install default-jdk-headless
RUN npm install -g bower pulp firebase-tools
USER pureuser

ENV PATH $HOME/purescript:$PATH
RUN wget -O $HOME/purescript.tar.gz https://github.com/purescript/purescript/releases/download/v0.12.2/linux64.tar.gz && \
   echo "5075eced1436d4d5f7a47823f5c4333c1f1d3edc $HOME/purescript.tar.gz" | sha1sum -c - && \
   tar -xvf $HOME/purescript.tar.gz -C $HOME/ && \
   rm $HOME/purescript.tar.gz
RUN chmod a+x $HOME/purescript
ADD package.json .
ADD bower.json .
RUN npm install
RUN bower install
ADD travis-build.sh .
COPY src src
COPY test test
RUN firebase emulators:exec --only database ./travis-build.sh
