set -e
service postgresql start

patch --output=snapshots/Main-2.hs snapshots/Main-1.hs 1-add-orville-state.patch
patch --output=snapshots/Main-3.hs snapshots/Main-2.hs 2-update-runApplication.patch
patch --output=snapshots/Main-4.hs snapshots/Main-3.hs 3-add-HasOrvilleState.patch
patch --output=snapshots/Main-5.hs snapshots/Main-4.hs 4-add-remaining-typeclasses.patch
mkdir -p src
cp snapshots/Main-5.hs src/Main.hs

stack build

expected=$(cat expected-output.txt)
actual=$(stack exec adding-orville-existing-readert)

if [ "$expected" = "$actual" ]; then
  echo "Output matches expected"
else
  echo "Expected output to be: $expected"
  echo "But it was actually  : $actual"
  exit 1
fi;
