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
if [ -n "$1" ]; then
    echo "$1" > "$DIR/COMMIT_HASH.txt"
fi
if [ -n "$2" ]; then
    echo "$2" > "$DIR/COMMIT_MSG.txt"
fi
