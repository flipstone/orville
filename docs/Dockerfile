FROM haskell:9.4.4-buster
RUN apt update && apt -y install python3-pip postgresql screen
RUN pip3 install --user prysk
RUN curl -Lo ~/.local/bin/mdsh https://github.com/bashup/mdsh/raw/master/bin/mdsh && chmod +x ~/.local/bin/mdsh
RUN cabal update
