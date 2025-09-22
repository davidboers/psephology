FROM haskell:9.12

# GHCup/GHC install
ARG GHC_VERSION=9.12.2
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN ghcup install ghc "${GHC_VERSION}"
RUN ghcup set ghc "${GHC_VERSION}"

# Cabal update
WORKDIR /project
RUN cabal update

COPY ./psephology.cabal /project/psephology.cabal
RUN cabal build --only-dependencies -j4

# Build everything, including tests
COPY . /project
RUN cabal build all --enable-tests

# Docs
RUN OUTPUT=$(cabal haddock 2>&1 | tee /dev/stderr) && \
    LAST_LINE=$(echo "$OUTPUT" | tail -n 1) && \
    DOCS_PATH=$(echo "$LAST_LINE" | awk '{print $NF}') && \
    mkdir -p docs && \
    rm -rf docs/* && \
    cp -r "$DOCS_PATH"/* docs/

CMD [ "cabal", "run", "tests" ]
