# SNIPPET: hidden
set -e
rm -rf orville-getting-started
service postgresql start
# SNIPPET: initProject
mkdir orville-getting-started
cd orville-getting-started
stack new orville-getting-started --bare simple --resolver lts-21.19
# SNIPPET: hidden
echo "system-ghc: true" >> stack.yaml
echo "install-ghc: false" >> stack.yaml
# SNIPPET: hidden
patch stack.yaml ../add-orville-extra-dep-to-stack-yaml.patch
# SNIPPET: hidden
patch orville-getting-started.cabal ../add-orville-to-cabal-file.patch
# SNIPPET: hidden
cp ../Main.hs src/Main.hs
# SNIPPET: buildAndExecute
stack build
stack exec orville-getting-started
# SNIPPET: hidden
expected=$(cat ../expected-output.txt)
actual=$(stack exec orville-getting-started)

if [ "$expected" = "$actual" ]; then
  echo "Output matches expected"
else
  echo "Expected output to be: $expected"
  echo "But it was actually  : $actual"
  exit 1
fi;
