set -e
service postgresql start
stack build
expected=$(cat expected-output.txt)
actual=$(stack exec hero)

if [ "$expected" = "$actual" ]; then
  echo "Output matches expected"
else
  echo "Expected output to be: $expected"
  echo "But it was actually  : $actual"
  exit 1
fi;
