#!/usr/bin/env python
import os, re, sys

rootdir = os.path.dirname(os.path.realpath(__file__))
sources = []

def extract_include(includes, root, line):
	match = re.search("^\\s*#\\s*include\\s*[\"][^>\"]*[\"]\\s*", line)
	if match != None:
		s = match.group(0)
		s = s[s.index('"')+1:s.rfind('"')]
		s = os.path.join(root, s)
		includes.append(s)
		return True
	match = re.search("^\\s*#\\s*include\\s*<lacc/[^>\"]*[\>]\\s*", line)
	if match != None:
		s = match.group(0)
		s = s[s.index('<')+1:s.rfind('>')]
		s = os.path.join(rootdir, "include", s)
		includes.append(s)
		return True
	return False

class Source:
	def __init__(self, root, name):
		self.root = root
		self.name = name
		self.lines = []
		self.includes = []
		self.path = os.path.join(root, name)
		with open(self.path) as f:
			for line in f:
				if not extract_include(self.includes, self.root, line):
					self.lines.append(line)

	def write(self, file):
		print "% 5d" % len(self.lines), self.path
		for inc in self.includes:
			print "  ", inc
		for line in self.lines:
			file.write(line)

def extract_unincluded():
	inc = None
	for i, source in enumerate(sources):
		if len(source.includes) == 0:
			del sources[i]
			inc = source
			break
	if inc != None:
		for source in sources:
			try:
				source.includes.remove(inc.path)
			except:
				pass
	return inc

def find_source_files(path):
	for root, _, files in os.walk(path):
		for f in files:
			if f.endswith(".h") or f.endswith(".c"):
				source = Source(root, f)
				sources.append(source)

if __name__ == "__main__":
	find_source_files(rootdir + "/include/lacc")
	find_source_files(rootdir + "/src")
	with open("bin/amalgamation.c", "w") as f:
		while len(sources) > 0:
			source = extract_unincluded()
			if source == None:
  				print >> sys.stderr, "Cyclic dependency"
				for source in sources:
					print source.path
					for dep in source.includes:
						print " -> ", dep
  				break
			source.write(f)
