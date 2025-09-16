DATE_STRING=$(date +%Y-%m-%d_%H-%M-%S)
DIR="roulette-benchmark-data/results/run_${DATE_STRING}"
mkdir "$DIR"
mv *.json "$DIR/"
