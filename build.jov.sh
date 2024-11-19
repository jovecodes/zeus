set -e
../probe/odin/odin build src -debug -out:zeus
./zeus ./test.zeus
./test
