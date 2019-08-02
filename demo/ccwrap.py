#!/usr/bin/env python

import sys, os
import subprocess

if __name__ == "__main__":
	args = sys.argv[:]
	args[0] = 'cc'
	if "-c" in args:
		args[0] = os.getenv('CC', 'cc')
		print " ".join(args)

	c = subprocess.call(args)
	sys.exit(c)
