import subprocess
import os

_exe_path_g = ""
_in_name_g = ""
_output_path_g=""
_count_g = 0

def _getDefaultArgs():
  global _in_name_g, _exe_path_g
  if _in_name_g == "" or _exe_path_g == "":
    raise Exception
  default_args = [_exe_path_g]
  # Unwind non-recursive functions
  default_args.append("-l")
  # One Return Location
  default_args.append("-r")
  # Function Calls use unique variable names as actual parameter
  default_args.append("-s")
  # Duplicate paratemeters for backup
  default_args.append("-e")

  return default_args

def setExePath(exe_path):
  global _exe_path_g
  _exe_path_g = exe_path

def setDesignPath(in_name):
  global _in_name_g
  _in_name_g = in_name

def setOutputPath(out_path):
  global _output_path_g
  _output_path_g = out_path
  # Clear output directory
  if os.path.exists(_output_path_g):
    for f in os.listdir(_output_path_g):
      os.remove(_output_path_g+'/'+f)
  else:
    os.makedirs(_output_path_g)

def createUnderApproxFile(uw_times):
  global _count_g, _in_name_g
  # Copy default arguments
  args = _getDefaultArgs()
  # Unwind main
  args.extend(["-w", "main", str(uw_times)])
  # Set Under-approximation for replacing function call
  # args.extend(["-m", func_name, "1", "0"])
  args.extend(["-u"])
  # Insert definition of assert function
  args.append("-i")
  # Set Output File
  file_name = "main" + str(_count_g).zfill(2) + ".under.c"
  output_file_name = _output_path_g + '/' + file_name
  args.extend(["-o", output_file_name])
  # Set Input File
  args.append(_in_name_g)
  # Call the process
  subprocess.call(args)
  _count_g = _count_g + 1
  return output_file_name

def createCheckSummaryFile(func_name, summary, uw_times):
  global _count_g, _in_name_g
  # Copy default arguments
  args = _getDefaultArgs()
  # Unwind designated function
  args.extend(["-w", func_name, str(uw_times)])
  # Insert definition of assert function
  args.append("-i")
  # Set Summary for replacing function call
  args.extend(["-m", func_name, "1", summary])
  # Set assertion to verify Summary
  args.extend(["-t", func_name, "1", summary])
  # Set Output File
  file_name = func_name + str(_count_g).zfill(2) + ".verify.c"
  output_file_name = _output_path_g + '/' + file_name
  args.extend(["-o", output_file_name])
  # Set Input File
  args.append(_in_name_g)
  # Call the process
  subprocess.call(args)
  _count_g = _count_g + 1
  return output_file_name

