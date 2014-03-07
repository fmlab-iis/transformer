
import algorithm
import cil_manager as cil_mgr
import verifier_manager as verifier_mgr
import verifier_basic_manager as v_basic_mgr
import redlog_manager as rl_mgr

def config(design_file, conf_parser):
  options = dict(conf_parser.items("general"))
  out_dir = options["output_dir"] + '/'
  tmp_dir = options["temp_dir"] + '/'

  # TODO Handle relative path problem
  op = conf_parser.getboolean("algorithm", "refine_summary")
  algorithm.setOptions("refine_summary", op)

  cil_mgr.setExePath("cil/tran.native")
  cil_mgr.setDesignPath(design_file)
  cil_mgr.setOutputPath(tmp_dir + "transformed")

  options = dict(conf_parser.items("verifier_gen_proof"))
  verifier_mgr.setExePath( options["bin_file"] )
  verifier_mgr.setConfigPath( options["conf_file"] )
  verifier_mgr.setOutputPath(tmp_dir + "proofs/")

  options = dict(conf_parser.items("verifier_basic"))
  v_basic_mgr._setPath("bin_file",  options["bin_file"])
  v_basic_mgr._setPath("conf_file", options["conf_file"])
  v_basic_mgr._setPath("out_dir",  tmp_dir + "proofs/")
  v_basic_mgr._init()

  options = dict(conf_parser.items("quantifier"))
  rl_mgr.setExePath( options["bin_file"] )
  rl_mgr.setScriptPath(tmp_dir + "rl_script")
  rl_mgr.setOutputPath("summary")

def run(design_file):
  algorithm.main(design_file)
