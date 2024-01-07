#!/usr/bin/env python
import sys
import os.path
import subprocess

LASTDIR=sys.argv[1]

#print("Current working directory: {0}\n".format(os.getcwd()))

print("Going to run tests in $PWD...")
PKG_OPAM=f"./{LASTDIR}.opam"
if not os.path.isfile(f"{PKG_OPAM}"):
  print(f"File {PKG_OPAM} does not exist. Exit\n")
  exit(1)

errors_found=False

# A few warnings were disabled
# 21: Field 'opam-version' doesn't match the current version
subpr = subprocess.Popen(["opam", "lint", "-s", "--warnings=-21-23", f"{PKG_OPAM}"], stdout=subprocess.PIPE)
if subpr.wait() != 0:
  print("Linting failed")
  subpr_rez = subpr.stdout.read().decode('utf-8')
  print(subpr_rez)
  exit(1)

def check_spec(field):
  pr = subprocess.Popen(["opam", "show", PKG_OPAM, "--field", field], stdout=subprocess.PIPE)
  if pr.wait() != 0:
    sys.stderr.write(f"Can't read field {field}\n")
    errors_found = True
  else:
    rez = pr.stdout.read().decode('utf-8').rstrip()
    if rez.find("FIXME") != -1:
      sys.stderr.write(f"Wrong value of the field '{field}': {rez}\n")
      if LASTDIR != "Lambda":
        errors_found = True
    else:
      print(f"decent value {rez}")

check_spec("synopsis")
check_spec("description")
check_spec("authors")
check_spec("maintainer")

if errors_found:
  exit(1)
else:
  exit(0)
