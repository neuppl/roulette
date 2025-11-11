from jinja2 import Environment, FileSystemLoader
import argparse
import json
from pathlib import Path

# Set up Jinja environment to load templates from the templates folder
env = Environment(loader=FileSystemLoader('templates'))

# Load the template
template = env.get_template('basic.html')


if __name__ == "__main__":
  parser = argparse.ArgumentParser(
                      prog='visualization tool for probabilistic profiler',
                      description='Provided a JSON output from interrupt profiler, visualizes the results in html format.')

  parser.add_argument('jsonfile')


  args = parser.parse_args()
  path = args.jsonfile

  with open(path, 'r') as file:
    data = json.load(file)
    file_path = data["file-path"]
    code = data["source-code"]
    heuristics = data["heuristics"]
    # Render the template with data
    html_output = template.render(
        file=file_path,
        code=code,
        heuristics=heuristics
    )

    save_path = Path(file_path).with_suffix(".html")
    # Save to file
    with open(save_path, 'w') as file:
        file.write(html_output)

    print("HTML file produced at: ", save_path)
