# System Modules
import sys
from ConfigParser import SafeConfigParser

# Avoid writing Python Bytecode
sys.dont_write_bytecode=True

# Project Modules
import engine# .verifier_manager as verifier_mgr

conf_parser = SafeConfigParser()
conf_parser.read("default.conf")

# TODO Command Line Parser
design_file = sys.argv[1]

engine.config( design_file, conf_parser )

engine.run( design_file )
