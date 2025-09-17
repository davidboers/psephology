set +xv
# Mirrors GitHub workflow
cabal build all --enable-tests
cabal run tests
cabal run example-generate-blt
cabal run example-georgia-redistricting
cabal run example-mrp
cabal run example-sample-parliament
bash docs.sh