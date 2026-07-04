# Find and run all .rkt files in the benchmark directory and its subdirectories
find . -type f -name '*.rkt' | while read -r benchmark; do
    echo "----------------------------------------"
    echo "Running benchmark: $benchmark"

    racket "$benchmark"
    echo "----------------------------------------\n"
done

echo "All benchmarks completed"


DATE_STRING=$(date +%Y-%m-%d_%H-%M-%S)
DIR="data/run_${DATE_STRING}"
mkdir -p "$DIR"
mv *.json "$DIR/" 
