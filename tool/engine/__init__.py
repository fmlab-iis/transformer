
import cil_manager as cil_mgr
import verifier_manager as verifier_mgr
import redlog_manager as rl_mgr

import algorithm

def config(design_file, conf_parser):
  options = dict(conf_parser.items("general"))
  out_dir = options["output_dir"] + '/'
  tmp_dir = options["temp_dir"] + '/'

  # TODO Handle relative path problem

  cil_mgr.setExePath("cil/tran.native")
  cil_mgr.setDesignPath(design_file)
  cil_mgr.setOutputPath(tmp_dir + "transformed")

  options = dict(conf_parser.items("verifier"))
  verifier_mgr.setExePath( options["bin_file"] )
  verifier_mgr.setConfigPath( options["conf_file"] )
  verifier_mgr.setOutputPath(tmp_dir + "proofs/")

  options = dict(conf_parser.items("quantifier"))
  rl_mgr.setExePath( options["bin_file"] )
  rl_mgr.setScriptPath(tmp_dir + "rl_script")
  rl_mgr.setOutputPath("summary")

def run(design_file):
  algorithm.main(design_file)
