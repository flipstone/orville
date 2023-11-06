# SNIPPET: hidden
set -e
service postgresql start
# SNIPPET: buildAndExecute
stack build
stack exec using-json
# SNIPPET: hidden
expected=$(cat expected-output.txt)
actual=$(stack exec using-json)

if [ "$expected" = "$actual" ]; then
  echo "Output matches expected"
else
  echo "Expected output to be: $expected"
  echo "But it was actually  : $actual"
  exit 1
fi;
