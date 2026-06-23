from jinja2 import Environment, FileSystemLoader
import argparse
import json
from pathlib import Path

env = Environment(loader=FileSystemLoader(Path(__file__).resolve().parent / "templates"))
template = env.get_template('results.html')

RESERVED_KEYS = {"file-path", "source-code", "Total-runs", "Collected-samples", "config"}


def syntactic_source_key(src):
    return (src["line"], src["column"], src["position"], src["span"])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='visualization tool for probabilistic profiler',
        description='Provided a JSON output from interrupt profiler, visualizes the results in html format.')
    parser.add_argument('jsonfile')
    args = parser.parse_args()

    with open(args.jsonfile, 'r') as file:
        data = json.load(file)

    file_path = data["file-path"]
    code = data["source-code"]
    total_runs = data.get("Total-runs", 0)
    total_samples = data.get("Collected-samples", 0)
    config = data.get("config", {})

    entries = {k: v for k, v in data.items() if k not in RESERVED_KEYS}

    scores = {k: v["cost-value"] for k, v in entries.items()}
    max_score = max(scores.values(), default=1) or 1.0

    # Build heuristics dict for the template.
    # color-ratio: 0 = cheapest (green), 1 = most expensive (red)
    heuristics = {"Total-runs": total_runs, "Collected-samples": total_samples}
    for k, v in entries.items():
        normalized = scores[k] / max_score
        heuristics[k] = {
            "color-ratio": round(normalized, 6),
            "num-successful-samples": v["results"]["num-successful-samples"],
            "num-total-samples": v["results"]["num-total-samples"],
            "total-recursive-calls": v["results"]["total-recursive-calls"],
        }

    # Group keys by syntactic-source to build the sources list.
    # Each source groups the context keys that share the same code span.
    source_groups = {}
    for k, v in entries.items():
        sk = syntactic_source_key(v["syntactic-source"])
        if sk not in source_groups:
            src = v["syntactic-source"]
            source_groups[sk] = {
                "line": src["line"],
                "column": src["column"],
                "position": src["position"],
                "span": src["span"],
                "contexts": {}
            }
        r = v["results"]
        value_str = (
            f"Successful samples: {r['num-successful-samples']}/{r['num-total-samples']}\n"
            f"Total recursive calls: {r['total-recursive-calls']}"
        )
        source_groups[sk]["contexts"][k] = {"value": value_str, "label": ""}

    sources = list(source_groups.values())

    html_output = template.render(
        file=file_path,
        code=code,
        heuristics=heuristics,
        sources=sources,
        config=config,
        num_flips=len(entries)
    )

    save_path = Path(file_path).with_suffix(".html")
    with open(save_path, 'w') as f:
        f.write(html_output)

    print("HTML file produced at: ", save_path)
