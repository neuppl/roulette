
#!/bin/bash

# Directory containing benchmark files
BENCHMARK_DIR="roulette/benchmark"

# Check if directory exists
if [ ! -d "$BENCHMARK_DIR" ]; then
    echo "Error: Directory $BENCHMARK_DIR not found"
    exit 1
fi

# Find and run all .rkt files in the benchmark directory
for benchmark in "$BENCHMARK_DIR"/*.rkt; do
    if [ -f "$benchmark" ]; then
        echo "----------------------------------------"
        echo "Running benchmark: $(basename "$benchmark")"

        racket "$benchmark"
        echo ""
    fi
done

echo "All benchmarks completed"