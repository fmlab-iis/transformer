import re
import linecache

import redlog_manager as rl_mgr

def _initialize():
  global _rl_op_dict_g, _rl_rex_g
  #TODO Support output format from reduce
  _rl_op_dict_g = {"and":"&&", "or":"||", "not":"!", "=":"==", "<>":"!="}
  _rl_rex_g = r" (?P<op>("
  for key in _rl_op_dict_g.keys():
    _rl_rex_g += "%s|" % key
  _rl_rex_g = _rl_rex_g[:-1] + r")) "

def useFormalPara(expr, func_call):
  idx = 0
  quantify_list = []
  rlvar_formal_map = {}
  match_obj = re.search("{(?P<var>[^{}]+)}", expr)
  while match_obj: # Find non-repeated variable names
    actual_str = match_obj.group("var")
    actual_name = (actual_str.split("::"))[1]
    formal_name = func_call.getFormalPara(actual_name)
    var_name = "rlvar_" + str(idx)
    idx += 1
    if formal_name != None:
      rlvar_formal_map[var_name] = formal_name
    else:
      quantify_list.append(var_name)
    # Replace by temporary variable name
    expr = re.sub(match_obj.group(0), var_name, expr)
    match_obj = re.search("{(?P<var>[^{}]+)}", expr)

  # Call redlog to quantify
  expr = rl_mgr.eliminateQuantifier(expr, quantify_list)
  # Replace by original variable names
  def replace_func(match_obj):
    formal_name = match_obj.group(0)
    assert formal_name in rlvar_formal_map
    return rlvar_formal_map[formal_name]
  expr = re.sub("rlvar_\d+", replace_func, expr)

  return '(' + redlogToCExpr(expr) + ')'

def redlogToCExpr(rl_expr):
  if rl_expr == "true":
    return "1"
  elif rl_expr == "false":
    return "0"
  return re.sub(_rl_rex_g, lambda x: _rl_op_dict_g[x.group("op")], rl_expr)

# TODO Seperate AbstractionReader and smt_parser
class SMTLibToRedlog:
  def __init__(self, file_name):
    self._abs_file_name = file_name
    # Check if the file is modified
    linecache.checkcache(self._abs_file_name)

    self._def_line_map = {}
    #Initial Definitions
    self._def_cexpr_map = {"true":"true", "false":"false"}

# Initialize this module
_initialize()
