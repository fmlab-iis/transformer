import os
import subprocess

from verifier_manager import ProofInfo

__paths_g = {}
__default_cmd_args = []

def _setPath(key, value):
  __paths_g[key] = value

def _init():
  global __paths_g, __default_cmd_args
  bin_file  = __paths_g["bin_file"]
  conf_file = __paths_g["conf_file"]
  out_dir   = __paths_g["out_dir"]

  if not os.path.exists(out_dir):
    os.makedirs(out_dir)
  else:
    for f in os.listdir(out_dir):
      os.remove(out_dir+f)

  __default_cmd_args = [bin_file]
  __default_cmd_args.extend(["-config", conf_file])
  __default_cmd_args.extend(["-outputpath", out_dir])

def __getDefaultArgs():
  global __paths_g
  out_dir = __paths_g["out_dir"]
  assert os.path.exists(out_dir)
  for f in os.listdir(out_dir):
    os.remove(out_dir+'/'+f)
  # Return copy
  return list(__default_cmd_args)

def verify(in_file, entry_func="main"):
  # Copy default arguments
  args = __getDefaultArgs()
  # Set Entry Function
  args.extend(["-entryfunction", entry_func])
  # Set design to be verified
  args.append(in_file);
  with open(os.devnull, "w") as null_file:
    # Call Verifier
    subprocess.call(args, universal_newlines=True, stdout=None, stderr=null_file)
  null_file.close()

  return ProofInfo(__paths_g["out_dir"]) 
