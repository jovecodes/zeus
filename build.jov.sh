set -e
# ../probe/odin/odin build src -debug -out:zeus
# ./zeus ./test.zeus
# ./test
#
../probe/odin/odin build src/stack/stack.odin -file -debug -out:stack
./stack
