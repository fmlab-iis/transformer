import fileinput
import re

def setRecursiveFuncList(design_name):
  #TODO Refine, this one is inefficient
  global _func_name_set_g
  _func_name_set_g = set()
  with open(design_name, "r") as design_file:
    for line in design_file:
      comment_begin = "// "
      m = re.search("// (?P<func_name>\w+);", line)
      if m != None:
        func_name = m.group("func_name")
        _func_name_set_g.add(func_name)
  design_file.close()

def getRecursiveFuncList():
  global _func_name_set_g
  return _func_name_set_g

class FuncCall:
  def __init__(self, func_str):
    func_call = func_str.split(';')
    self.name = func_call[0]
    self.para_map = {}
    for pair_str in [func_call[1]] + func_call[2].split(','):
      formal, actual = pair_str.split('=')
      self.para_map[actual] = formal
    self.depth = int((func_call[3].split('='))[1])
    # FIXME Need a robust way to acquire location of unwinded function call
    self.begin_loc = int(func_call[4]) - 2
    self.end_loc = int(func_call[5]) + 2
  def getFormalPara(self, var_name):
    # TODO Find back up of initial value
    if var_name in self.para_map:
      return self.para_map[var_name]
    return None

class TransformedDesignInfo:
  def __init__(self, design_name):
    self._design_name = design_name
    self._parseUnwindedCalls()

  def _parseUnwindedCalls(self):
    uw_call_list = []
    stack = []
    design_file = fileinput.input(self._design_name)
    for line in design_file:
      match_obj = re.search("// Unwind (?P<indicator>[^:]+): (?P<func_name>\w+);", line)
      if match_obj != None:
        if match_obj.group("func_name") in _func_name_set_g:
          if match_obj.group("indicator") == "Begin":
            str_func = (line[match_obj.start("func_name"):]).strip(" \n")
            stack.append(str_func+';'+str(design_file.filelineno()))
          else:
            assert match_obj.group("indicator") == "End"
            assert stack
            str_func = stack.pop() + ';' + str(design_file.filelineno())
            uw_call_list.append(str_func);
    design_file.close()
    assert not stack
    self._uw_call_list = map(FuncCall, uw_call_list)
    # Only care unwinded call with depth == 1
    self._uw_call_list = filter(lambda x: x.depth==1, self._uw_call_list)

