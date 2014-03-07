import fileinput
import linecache
import os
import re
import subprocess

import arg_utils
import smtlib_parser
from smtlib_parser import tokenize, toCOperator

_exe_path_g = ""
_conf_path_g = ""
_out_path_g = ""

def _getDefaultArgs():
  global _exe_path_g, _conf_path_g, _out_path_g
  assert os.path.exists(_out_path_g)
  # Clear output directory
  for f in os.listdir(_out_path_g):
    os.remove(_out_path_g+'/'+f)

  default_args = [_exe_path_g]
  default_args.extend(["-config", _conf_path_g])
  default_args.extend(["-outputpath", _out_path_g])
  return default_args

def setExePath(exe_path):
  global _exe_path_g
  _exe_path_g = exe_path
def setConfigPath(conf_path):
  global _conf_path_g
  _conf_path_g = conf_path
def setOutputPath(out_path):
  global _out_path_g
  _out_path_g = out_path
  if not os.path.exists(_out_path_g):
    os.makedirs(_out_path_g)
  else:
    for f in os.listdir(_out_path_g):
      os.remove(_out_path_g+'/'+f)

def verify(in_file, entry_func="main"):
  # Copy default arguments
  args = _getDefaultArgs()
  # Set Entry Function
  args.extend(["-entryfunction", entry_func])
  # Set design to be verified
  args.append(in_file);
  with open(os.devnull, "w") as null_file:
    # Call Verifier
    subprocess.call(args, universal_newlines=True, stdout=None, stderr=null_file)
  null_file.close()
  return ProofInfo(_out_path_g) 

# TODO Seperate ProofInfo to a standalone file 
class ProofInfo:
  def __init__(self, proof_dir):
    self._proof_dir = proof_dir + '/'
  def getResult(self):
    # FIXME This way is inefficient.
    with open(self._proof_dir+"Statistics.txt", "r") as stats:
      for line in stats:
        pattern = "Verification result: "
        if line.startswith(pattern):
          begin = len(pattern)
          result = (re.search(r"(?P<result>^[A-Z]+)",line[begin:])).group("result")
    stats.close()
    return result
  def getARGHandler(self):
    file_name = self._proof_dir+"ARG.dot"
    return arg_utils.ARGHandler(file_name)
  def getAbstractionReader(self, abs_id_list):
    # FIXME Raise Exception
    file_name = self._proof_dir+"abstractions.txt"
    return AbstractionReader(file_name, abs_id_list)
  def getCEXReader(self):
    file_name = self._proof_dir+"ErrorPath.0.assignment.txt"
    return CEXReader(file_name)

class CEXReader:
  def __init__(self, file_name):
    self._assign_file_name = file_name
  def getFormalParaValues(self, func_call):
    var_value_map = {}
    formal_set = func_call.getFormalParaSet()
    with open(self._assign_file_name, "r") as assignment_file:
      for line in assignment_file:
        match_obj = re.match("^(?P<var>\w+::\w+)@\d+ : (?P<type>\w+): (?P<value>[+-]?\d+(?:\.\d+)?)", line)
        if not match_obj:
          continue
        var_str = (match_obj.group("var")).split("::")
        var_scope = var_str[0]
        if var_scope != func_call.name:
          continue
        var_name = var_str[1]
        if var_name in formal_set and var_name not in var_value_map:
          value_str = match_obj.group("value")
          var_value_map[var_name] = value_str
          if len(formal_set) == len(var_value_map): break;
    assignment_file.close()
    return var_value_map

class AbstractionReader:
  def __init__(self, file_name, abs_id_list):
    self._abs_file_name = file_name
    self._def_line_map = {}
    #Initial Definitions
    self._def_rlexpr_map = {"true":"true", "false":"false"}
    self._id_def_map = dict.fromkeys(set(abs_id_list))
    # Check if the file is modified
    linecache.checkcache(self._abs_file_name)
    self._readFile()
    for abs_id, def_id in self._id_def_map.iteritems():
      cexpr = self._buildCExprFromDef(def_id)
    self._def_line_map.clear()

  def translateAbstraction(self, abs_id):
    assert abs_id in self._id_def_map
    def_id = self._id_def_map[abs_id]
    assert def_id in self._def_rlexpr_map
    cexpr = self._def_rlexpr_map[def_id]
    return cexpr

  def _readFile(self):
    abs_file = fileinput.input(self._abs_file_name)
    # Former part contains SMTLib2 commands
    for line in abs_file:
      if( not smtlib_parser.isSMTLibCommand(line) ):
        assert line == "\n"
        break
      if( smtlib_parser.isDefineFunction(line) ):
        def_name = smtlib_parser.getDefineFunctionName(line)
        self._def_line_map[def_name] = abs_file.filelineno()
    # Latter part is a map from ABS_ID to defined function.
    for line in abs_file:
      assert not smtlib_parser.isSMTLibCommand(line)
      abs_id_str = (line.split(" ",1))[0]
      assert abs_id_str.isdigit()
      abs_id = int(abs_id_str)
      next_line = next(abs_file)
      assert smtlib_parser.isAssert(next_line)
      if( self._id_def_map.has_key( abs_id ) ):
        def_name = smtlib_parser.getAssertName(next_line)
        self._id_def_map[abs_id] = def_name
      next_line = next(abs_file)
      assert next_line == "\n"
    abs_file.close()

  def _buildCOperand(self, operand):
    assert type(operand) == str or type(operand) == list
    if type(operand) == list:
       return self._buildCExprFromList(operand)

    if operand in self._def_line_map :
      ret = self._buildCExprFromDef(operand)
    elif operand.startswith("|"):
      assert operand.endswith("|")
      ret = "{" + (operand.strip("|")) + "}"
    else :
      ret = operand
    return ret

  def _buildCExprFromList(self, prefix_list):
    assert len(prefix_list)==2 or len(prefix_list)==3
    # Parse Operator and First Operand
    cexpr_op = toCOperator(prefix_list[0])
    cexpr_l = self._buildCOperand(prefix_list[1])
    if( len(prefix_list)==3 ):
      # Parse Second Operand
      cexpr_r = self._buildCOperand(prefix_list[2])
      ret = cexpr_l+cexpr_op+cexpr_r
    else:
      ret = cexpr_op + cexpr_l
    return '('+ ret +')'

  def _buildCExprFromDef(self, smt_def):
    if(smt_def in self._def_rlexpr_map):
      return self._def_rlexpr_map[smt_def]
    assert smt_def in self._def_line_map
    line = linecache.getline(self._abs_file_name, self._def_line_map[smt_def])
    # FIXME This is an extremely simple way to get formulae. Refine Later
    tmp_list = tokenize((line.strip(" \n")))
    prefix_list = tmp_list[4]
    ret = self._buildCExprFromList(prefix_list)
    # Record translated expressions
    self._def_rlexpr_map[smt_def] = ret
    return ret;

