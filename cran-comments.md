1.0-6 corrects some critical coding errors that were discovered after deployment of 1.0-5

## Test environments
* local macOS Catalina 10.15.3, R 3.6.2 (2019-12-12)
* local ubuntu 18.04, R 3.6.2 (2019-12-12)
* rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub Ubuntu Linux 16.04 LTS, R-release, GCC
* rhub Fedora Linux, R-devel, clang, gfortran
* rhub Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder Windows x86_64-w64-mingw32 (64-bit), 
    R-devel x86_64-w64-mingw32 (64-bit)

## local (R CMD) check results
There were no ERRORs, WARNINGs or NOTEs.

## rhub check results
There were no ERRORs or WARNINGs.  

One NOTE on Windows Server 2008 R2 SP1, R-devel, 32/64 bit:
Found the following files/directories:
  'hier.part-Ex_x64.Rout'
  'examples_i386' 'examples_x64' 'hier.part-Ex_i386.Rout'
rhub false positive?

## win-builder check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of hier.part
using revdepcheck::revdep_check(), and found no problems.
