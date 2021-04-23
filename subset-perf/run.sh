mkdir O0
mkdir O1
mkdir O2
echo "Compiling..."
ghc bench -O0 -outputdir O0 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
ghc bench -O1 -outputdir O1 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
ghc bench -O2 -outputdir O2 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
echo "Benchmarking..."
touch bench.txt
echo "BENCHMARK\n\n" > bench.txt
echo "O0\n\n" >> bench.txt
./O0/bench --output bench-O0.html >> bench.txt
echo "O1\n\n" >> bench.txt
./O1/bench --output bench-O1.html >> bench.txt
echo "O2\n\n" >> bench.txt
./O2/bench --output bench-O2.html >> bench.txt
