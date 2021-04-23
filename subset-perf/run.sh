ghc -O2 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
./bench --output bench.html
chromium-browser bench.html
