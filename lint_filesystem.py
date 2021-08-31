#/usr/bin/python
import sys
import os
import subprocess

print("Going to run tests in $PWD...")
LASTDIR=sys.argv[1]
print(LASTDIR)
PKG_OPAM=f'{LASTDIR}.opam'
print(PKG_OPAM)

if not os.path.isfile(PKG_OPAM):
  exit(1)

result = subprocess.run(['opam', 'lint', '-s', PKG_OPAM], stdout=subprocess.PIPE)
print(result.stdout.decode('utf-8').rstrip().split(' '))
exit(2)

pairs = [(23, 1), (25, 2), (35, 3), (36,1) ]
for code,bad_val in pairs:
  result = subprocess.run(['opam', 'lint', f'--warnings=+{code}', PKG_OPAM], stdout=subprocess.PIPE)
  if result.returncode != 0:
    print(result.stdout)
