#!/usr/bin/env python3
import sys
import os

LANG=sys.argv[1]

REPORTS_DIR="_reports"
REPORT_FILE="jscpd_report.txt"
def good_dir(d):
  return os.path.isdir(os.path.join(".", d)) and not d.startswith('.') and d != LANG and d != REPORTS_DIR

if not os.path.exists("_reports"):
  os.mkdir("_reports")
os.system("find . -iname _build -exec rm -fr {} \;")

NPM_BIN='/home/user/.local/bin'
for x in [x for x in os.listdir(".") if good_dir(x)]:
  cmd = f"{NPM_BIN}/jscpd --pattern '{LANG}/**/*.ml*' --pattern '{x}/**/*.ml*' -b -r consoleFull --skipLocal > _reports/vs_{x}.txt"
  print(cmd)
  os.system(cmd)

if 1:
  print("Looking for clones in itself");
  cmd = f"jscpd --pattern '{LANG}/**/*.ml*' -b -r consoleFull > _reports/vs_{LANG}.txt"
  os.system(cmd)

os.system(f"cat {REPORTS_DIR}/*.txt > {REPORT_FILE}")
