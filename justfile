[private]
default:
    @just --list --unsorted

# Build the projects
build:
    cabal build all

# Format the cabal files and the Haskell sources
style:
    cabal-gild floreal-prelude/floreal-prelude.cabal
    cabal-gild floreal-commons/floreal-commons.cabal
    fourmolu --mode inplace floreal-prelude floreal-commons

# Check formatting
style-check:
    fourmolu --mode check floreal-prelude floreal-commons
