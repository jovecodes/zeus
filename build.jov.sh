set -e
../probe/odin/odin build ./zeus.odin -file
./zeus ./test.zeus
./test
