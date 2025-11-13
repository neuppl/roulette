from jinja2 import Environment, FileSystemLoader
import argparse
import json
from pathlib import Path

# Set up Jinja environment to load templates from the templates folder
env = Environment(loader=FileSystemLoader('templates'))

# Load the template
template = env.get_template('results.html')



def same_syntactic_source(src1, src2):
   src1_tuple = tuple(sorted(src1.items()))
   src2_tuple = tuple(sorted(src2.items()))

   return (src1 == src2)

   
def unique_dicts(list_of_dicts):
    seen = set()
    unique_list = []
    
    for d in list_of_dicts:
        # Convert dict to a frozenset of its items (hashable)
        dict_tuple = tuple(sorted(d.items()))
        if dict_tuple not in seen:
            seen.add(dict_tuple)
            unique_list.append(d)
    
    return unique_list



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
    stack_contexts = data["stack-contexts"]

    unique_syntactic_sources = unique_dicts([v["syntactic-source"] for k,v in stack_contexts.items()])



    for src in unique_syntactic_sources:
      temp = {k:v["context"]
              for k,v in stack_contexts.items()
              if same_syntactic_source(v["syntactic-source"], src)}
      src["contexts"] = temp
    
    for src in unique_syntactic_sources:
       del src["source"]
      
      #del src['source'] # unnecessary file path, not used.

    # Render the template with data
    html_output = template.render(
        file=file_path,
        code=code,
        heuristics=heuristics, 
        sources = unique_syntactic_sources
    )

    save_path = Path(file_path).with_suffix(".html")
    # Save to file
    with open(save_path, 'w') as file:
        file.write(html_output)

    print("HTML file produced at: ", save_path)
