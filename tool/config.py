
from ConfigParser import SafeConfigParser
import logging

import engine

def config(design_file):
  conf_parser = SafeConfigParser()
  conf_parser.read("default.conf")

  engine.config( design_file, conf_parser )

  options = dict(conf_parser.items("output"))
  __initReporter(options)
   

def __initReporter(options):
  out_dir = options["output_dir"] + '/'
  log_file = out_dir + options["log_file"]
  logging.basicConfig(filename=log_file, filemode='w', level=logging.INFO)
