# Find and run all .rkt files in the benchmark directory
for benchmark in *.rkt; do
    if [ -f "$benchmark" ]; then
        echo "----------------------------------------"
        echo "Running benchmark: $(basename "$benchmark")"

        racket "$benchmark"
        echo "----------------------------------------\n"
    fi
done

echo "All benchmarks completed"


DATE_STRING=$(date +%Y-%m-%d_%H-%M-%S)
DIR="data/run_${DATE_STRING}"
mkdir -p "$DIR"
mv *.json "$DIR/" 

raco scribble --htmls results.scribl