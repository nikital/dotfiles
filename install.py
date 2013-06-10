#!/usr/bin/env python

import os
import sys

SUCCESS = 0
FAILURE = 1

HOME = os.path.expanduser('~')
IGNORED_FILES = [
	'.git',
	'.gitignore',
	'.gitmodules',
]

def list_dotfiles(path):
	return list(i for i in os.listdir(path) if (i[0] == '.') and (i not in IGNORED_FILES))

def check_conflicts():
	conflict_found = False
	for item in list_dotfiles('.'):
		if os.path.exists(os.path.join(HOME, item)):
			print '{0} exists in home directory!'.format(item)
			conflict_found = True
	return conflict_found

def make_symlinks():
	for item in list_dotfiles('.'):
		print 'Symlinking {0}...'.format(item)
		os.symlink(os.path.relpath(item, HOME), os.path.join(HOME, item))

def main():
	if check_conflicts() == True:
		return FAILURE
	make_symlinks()
	return SUCCESS

if __name__ == '__main__':
	sys.exit(main())
