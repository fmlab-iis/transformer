import sys
import time
# Avoid writing Python Bytecode
sys.dont_write_bytecode=True

import cil_manager as cil_mgr
import verifier_manager as verifier_mgr
import redlog_manager as rl_mgr
import design_analysis
import proof_analysis

def initialize(design_path):
  # Set Initial Configuration
  cil_mgr.setExePath("./cil/tran.native")
  cil_mgr.setDesignPath(design_path)
  cil_mgr.setOutputPath("transformed/")
  verifier_mgr.setExePath("./verifier/scripts/cpa.sh")
  verifier_mgr.setConfigPath("config/myCPA-PredAbstract-LIA.properties")
  verifier_mgr.setOutputPath("proofs/")
  rl_mgr.setExePath("redlog/reduce")
  rl_mgr.setScriptPath("redlog/cmd.in")
  rl_mgr.setOutputPath("summary")

  out_name = cil_mgr.create_under_approx_file(0)
  design_analysis.setRecursiveFuncList(out_name)

def main(argv):
  tStart = time.time()

  design_path = argv[1]
  printProcessStatus("Parsing Design " + design_path)
  initialize( design_path )
  print design_analysis.getRecursiveFuncList()

  uw_count = 0
  result = "CONTINUE"
  while time.time()-tStart < 900 and result == "CONTINUE":
    uw_count+=1
    result = oneIteration(uw_count)

  if result == "CONTINUE": result = "UNKNOWN"

  # TODO Print results and statistics
  printProcessStatus("Verification Result")
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
  return

def oneIteration(unwind_times):
  printProcessStatus("Create Under-approximation of main()")
  out_file_name = cil_mgr.create_under_approx_file(unwind_times)

  printProcessStatus("Verify Under-approximation of main()")
  proof_info = verifier_mgr.verify(out_file_name)

  result = proof_info.getResult()
  if not result == "SAFE":
    return result

  printProcessStatus("Guess Summaries")
  design_info = design_analysis.TransformedDesignInfo(out_file_name)
  # sum_list = guessSummary(proof_info, out_file_name)
  sum_list = proof_analysis.guessSummaries(design_info, proof_info)
  for func_name, summary in sum_list:
    print func_name + ": " + summary

  printProcessStatus("Verify Summaries")
  for func_name, summary in sum_list:
    out_file_name = cil_mgr.create_verify_summary_file(func_name, summary, unwind_times-1)
    proof = verifier_mgr.verify(out_file_name, func_name)
    result = proof.getResult()
    if result != "SAFE": break

  if result == "SAFE":
    return "SAFE"
  # Finished one iteration, continue on next
  return "CONTINUE"

def printProcessStatus(string):
  print 
  print (' '+string+' ').center(70, '=')
  print

if  __name__ == '__main__':
  main(sys.argv)

