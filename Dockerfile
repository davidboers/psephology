# This Dockerfile is intended for use by CI.

FROM ubuntu:latest

# GHCup/GHC install
ARG GHC_VERSION=9.14.1
RUN apt-get update && apt-get install -y curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN ghcup install ghc "${GHC_VERSION}"
RUN ghcup set ghc "${GHC_VERSION}"

# Build everything, including tests
COPY . .