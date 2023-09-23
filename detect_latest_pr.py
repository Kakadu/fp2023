#!/usr/bin/env pyhton3

import subprocess
import sys

result = subprocess.run(['sh', '-c', 'git remote | grep upstream'], stdout=subprocess.PIPE)
if result.returncode != 0:
    subprocess.run("git remote add upstream https://github.com/Kakadu/fp2023.git")
    subprocess.run("git fetch upstream master")

print(sys.argv[1])
result = subprocess.run(['git', 'merge-base', 'upstream/master', sys.argv[1]], stdout=subprocess.PIPE)
commit = result.stdout.decode('utf-8').partition('\n')[0]
print(commit)


result = subprocess.run(['sh', '-c', f'git diff-tree {commit}..{sys.argv[1]} | rev | cut -f 1 | rev'], stdout=subprocess.PIPE)
print(result)
changedPaths = result.stdout.decode('utf-8')

changedFiles = [line for line in changedPaths.split('\n') if line.strip()]
changedFiles = list(set(changedFiles))
print(changedFiles)




# while inotifywait -e close_write detect_latest_pr.py; do python3 ./detect_latest_pr.pymaster; done
