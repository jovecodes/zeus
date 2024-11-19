set -e
../probe/odin/odin build . -debug
./zeus ./test.zeus
./test
