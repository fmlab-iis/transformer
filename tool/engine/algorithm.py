import time
import fileinput
import logging

import cil_manager as cil_mgr
import verifier_manager as verifier_mgr
import verifier_basic_manager as v_basic_mgr
import design_analysis
import proof_analysis

_options_g = {}

def setOptions(key, value):
  _options_g[key] = value

def main(design_path):
  tStart = time.time()

  printProcessStatus("Parsing Design " + design_path)
  out_name = cil_mgr.createUnderApproxFile(0)
  design_analysis.setRecursiveFuncList(out_name)
  print "Recursive Functions:"
  print "  " + str(list(design_analysis.getRecursiveFuncList()))

  uw_count = 0
  result = "CONTINUE"
  while time.time()-tStart < 900 and result == "CONTINUE":
    uw_count+=1
    result = oneIteration(uw_count)

  if result == "CONTINUE": result = "UNKNOWN"

  # TODO Print results and statistics
  printProcessStatus("Return Result")
  print "Result: " + result
  if result == "SAFE":
    # TODO Proved SAFE. Provide SAFE proof
    "SAFE"
  elif result == "UNSAFE":
    # TODO Provide CEX
    "UNSAFE"

  tUsage = time.time() - tStart
  print "Iterations: " + str(uw_count)
  print "Time Usage: " + str(round(tUsage, 2)) + " sec."
  return result

def oneIteration(unwind_times):
  printProcessStatus("Create Under-approximation of main()")
  under_design_name = cil_mgr.createUnderApproxFile(unwind_times)

  printProcessStatus("Verify Under-approximation of main()")
  proof_info = verifier_mgr.verify(under_design_name)
  under_result = proof_info.getResult()
  if not under_result == "SAFE":
    return under_result

  printProcessStatus("Guess Summaries")
  sum_list = guessSummaries(under_design_name, proof_info)

  check_result = "UNKNOWN"
  can_refine = True
  while check_result != "SAFE" and can_refine:
    printProcessStatus("Check Summaries")
    check_result = "UNKNOWN"
    for func_call, summary in sum_list:
      func_name = func_call.name
      check_summary_name = cil_mgr.createCheckSummaryFile(func_name, summary, unwind_times-1)
      proof = v_basic_mgr.verify(check_summary_name, func_name)
      check_result = proof.getResult()
      if check_result != "SAFE": break
    can_refine = False
    if check_result == "UNSAFE" and _options_g["refine_summary"]:
      printProcessStatus("Refine Summaries")
      can_refine, sum_list=refineSummaries(under_design_name, func_call, proof)

  if check_result == "SAFE":
    # TODO
    print "Summary For Proving:"
    for func_call, summary in sum_list:
      print "  " + func_call.name + ": " + summary
    return "SAFE"
  # Finished one iteration, continue on next
  return "CONTINUE"

def refineSummaries(under_name, func_call, unsafe_proof):
  # TODO Refine pre-condition with the CEX of invalid summary
  cex_reader = unsafe_proof.getCEXReader()
  var_value_map = cex_reader.getFormalParaValues(func_call)
  cexpr = "("
  for var, value in var_value_map.items():
    cexpr += func_call.getActualPara(var) + "==" + value + "&&"
  cexpr = cexpr[:-len("&&")] + ")"

  c_stmt = " __VERIFIER_assert(!%s);\n" % cexpr
  # Insert assertion
  # FIXME Not robust
  old_file =fileinput.input(under_name) 
  new_name = under_name[:-len("under.c")] + "refine.c"
  with open(new_name, "w") as new_file:
    for line in old_file:
      new_file.write(line)
      if old_file.filelineno() == func_call.begin_loc:
        line = next(old_file)
        new_file.write( line.strip('\n') + c_stmt )
  new_file.close()
  old_file.close()

  proof_info = verifier_mgr.verify(new_name)
  new_result = proof_info.getResult()
  if not new_result == "SAFE":
    # Cannot refine, return False
    return False, []

  printProcessStatus("Refined Summaries")
  sum_list = guessSummaries(new_name, proof_info)
  return True , sum_list

def guessSummaries(design_name, proof_info):
  design_info = design_analysis.TransformedDesignInfo(design_name)
  sum_list = proof_analysis.guessSummaries(design_info, proof_info)
  for func_call, summary in sum_list:
    logging.info("Function Name: " + func_call.name)
    logging.info("  Guess Summary: "+ summary)
  return sum_list

def printProcessStatus(string):
  logging.info( (' '+string+' ').center(70, '=') )

