import sys
# Avoid writing Python Bytecode
sys.dont_write_bytecode=True

import config
import engine

# TODO Command Line Parser
design_file = sys.argv[1]

# Set Configurations
config.config(design_file)

# Main Procedure
proof = engine.run( design_file )

