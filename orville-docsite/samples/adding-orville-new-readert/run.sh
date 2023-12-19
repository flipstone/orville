# SNIPPET: hidden
set -e
service postgresql start
# SNIPPET: hidden
patch --output=snapshots/Main-2.hs snapshots/Main-1.hs 1-add-readert.patch
patch --output=snapshots/Main-3.hs snapshots/Main-2.hs 2-add-orville-typeclasses.patch
patch --output=snapshots/Main-4.hs snapshots/Main-3.hs 3-update-runApplication.patch
patch --output=snapshots/Main-5.hs snapshots/Main-4.hs 4-update-main.patch
patch --output=snapshots/Main-6.hs snapshots/Main-5.hs 5-add-table.patch
patch --output=snapshots/Main-7.hs snapshots/Main-6.hs 6-add-migrations.patch
mkdir -p src
cp snapshots/Main-7.hs src/Main.hs
# SNIPPET: buildAndExecute
stack build
# SNIPPET: hidden
expected=$(cat expected-output.txt)
actual=$(stack exec adding-orville-new-readert)

if [ "$expected" = "$actual" ]; then
  echo "Output matches expected"
else
  echo "Expected output to be: $expected"
  echo "But it was actually  : $actual"
  exit 1
fi;
