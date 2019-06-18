FROM ocaml/opam2:4.07

RUN sudo apt-get update && \
  sudo apt-get install -y m4 pkg-config

# update opam repository
RUN git pull && \
  opam update && \
  opam install dune

USER root
COPY . /flora
WORKDIR /flora

RUN eval $(opam env) && eval "$(dune external-lib-deps --missing src/app/main.exe 2>&1 >/dev/null | grep try | sed 's/.*try: //')"

ENTRYPOINT ["./flora"]