echo "Compiling O0..."
mkdir O0
ghc bench -O0 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench O0
echo "Compiling O1..."
mkdir O1
ghc bench -O1 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench O1
echo "Compiling O2..."
mkdir O2
ghc bench -O2 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench O2
echo "Benchmarking..."
touch bench.txt
echo "BENCHMARK\n" > bench.txt
echo "O0\n" | tee -a bench.txt
./O0/bench --output bench-O0.html >> bench.txt
echo "O1\n" | tee -a bench.txt
./O1/bench --output bench-O1.html >> bench.txt
echo "O2\n" | tee -a bench.txt
./O2/bench --output bench-O2.html >> bench.txt
