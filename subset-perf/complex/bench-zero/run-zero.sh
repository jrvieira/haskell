echo "Compiling O0..."
mkdir O0
ghc bench-zero -O0 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench-zero O0
echo "Compiling O1..."
mkdir O1
ghc bench-zero -O1 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench-zero O1
echo "Compiling O2..."
mkdir O2
ghc bench-zero -O2 -fforce-recomp -j -no-keep-hi-files -no-keep-o-files
mv bench-zero O2
echo "Benchmarking..."
touch bench-zero.txt
echo "BENCHMARK-zero (picoseconds, implementation dependent precision)" > bench-zero.txt
echo "\nO0\n" | tee -a bench-zero.txt
./O0/bench-zero >> bench-zero.txt
echo "\nO1\n" | tee -a bench-zero.txt
./O1/bench-zero >> bench-zero.txt
echo "\nO2\n" | tee -a bench-zero.txt
./O2/bench-zero >> bench-zero.txt
