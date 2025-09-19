FROM haskell:9.12

ARG GHC_VERSION=9.12.2
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN ghcup install ghc "${GHC_VERSION}"
RUN ghcup set ghc "${GHC_VERSION}"

WORKDIR /project
RUN cabal update

COPY ./psephology.cabal /project/psephology.cabal
RUN cabal build --only-dependencies -j4

COPY . /project
RUN cabal build all --enable-tests
RUN chmod +x /project/docs.sh && /project/docs.sh

CMD [ "cabal", "run", "tests" ]