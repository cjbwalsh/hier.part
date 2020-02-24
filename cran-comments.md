## Test environments
* local macOS Catalina 10.15.3, R 3.6.2 (2019-12-12)
* ubuntu 18.04 (on travis-ci), R 3.6.2 (2019-12-12)
* rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub Ubuntu Linux 16.04 LTS, R-release, GCC
* rhub Fedora Linux, R-devel, clang, gfortran
* rhub Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## rhub check results
There were no ERRORs or WARNINGs
Two NOTEs on Windows Server:

1. Examples with CPU (user + system) or elapsed time > 5s
for arch 'i386' and arch 'x64'

2. Found the following files/directories:
    'hier.part-Ex_x64.Rout'

## Downstream dependencies
I have also run R CMD check on downstream dependencies of hier.part
using revdepcheck::revdep_check(), and found no problems.