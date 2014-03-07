
import expression_utils as expr_utils

class FuncSummary:
  def __init__(self, func_call):
    self._call = func_call
    self._post_list = []
    self._sum_list = []
  def collectPostCond(self, post_cond):
    self._post_list.append(post_cond)
  def setPreCond(self, pre_cond):
    ref = self._post_list
    self._post_list = []
    self._sum_list.append( (pre_cond, ref) )
  def getSummaryList(self):
    return self._sum_list

def guessSummaries(design_info, proof_info):
  analyzer = ProofAnalyzer(proof_info)

  analyzer.parsePrePostPair(design_info._uw_call_list)

  sum_list = []
  for uw_call in design_info._uw_call_list:
    sum_list.append((uw_call, analyzer.getSummary(uw_call)))
  return sum_list
  

class ProofAnalyzer:
  def __init__(self, proof_info):
    self._proof_info = proof_info;
    self._func_sum_dict = {}
    self._needed_abs_id_list = None

  def parsePrePostPair(self, func_list):
    begin_loc_set = set([x.begin_loc for x in func_list])
    end_loc_dict = {x.end_loc : x.begin_loc for x in func_list}

    arg_handler = self._proof_info.getARGHandler() 

    abs_id_list = []
    for u, v, line_no in arg_handler.traverseEdgePostOrder():
      if line_no in end_loc_dict:
        begin_loc = end_loc_dict[line_no]
        if begin_loc not in self._func_sum_dict:
          # Create a new summary pair
          self._func_sum_dict[begin_loc] = FuncSummary(begin_loc)
        care_node = u 
        abs_id = arg_handler.getNodeABSId(care_node)
        abs_id_list.append(abs_id)
        self._func_sum_dict[begin_loc].collectPostCond(abs_id)
      elif line_no in begin_loc_set:
        begin_loc = line_no
        if begin_loc not in self._func_sum_dict:
          self._func_sum_dict[begin_loc] = FuncSummary(begin_loc)
        care_node = v
        abs_id = arg_handler.getNodeABSId(care_node)
        abs_id_list.append(abs_id)
        self._func_sum_dict[begin_loc].setPreCond(abs_id)
    self._needed_abs_id_list = abs_id_list

  def getSummary(self, func_call):
    abs_reader = self._proof_info.getAbstractionReader(self._needed_abs_id_list)

    sum_list = (self._func_sum_dict[func_call.begin_loc]).getSummaryList()
    ret_sum = ""
    for pre_id, post_id_list in sum_list:
      pre_cexpr=abs_reader.translateAbstraction(pre_id)
      post_cexpr = "false"
      # Disjunct all post-conditions
      for post_id in post_id_list:
        post_cexpr += " or " + abs_reader.translateAbstraction(post_id)

      if pre_cexpr == "true":
        sum_cexpr = post_cexpr
      else:
        sum_cexpr = "(" + pre_cexpr + " impl " + post_cexpr+')'
      ret_sum += sum_cexpr + " and "
    ret_sum = ret_sum[:-len(" and ")]
    ret_sum = expr_utils.useFormalPara(ret_sum, func_call)
    return ret_sum

