from ubuntu

RUN apt update
RUN apt install curl -y
RUN curl -o ghcup https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup 
RUN chmod +x ghcup
RUN mv ghcup bin/
ENV GHCUP_INSTALL_BASE_PREFIX=/
RUN ghcup install stack
# RUN "export PATH=$PATH:.ghcup/bin/"
ENV PATH="${PATH}:/.ghcup/bin"

RUN apt install -y build-essential libgmp-dev

COPY . /haskell-project/
WORKDIR /haskell-project
RUN apt install -y pkg-config zlib1g-dev libbz2-dev
RUN stack build

# ENTRYPOINT ["stack", "run"]
