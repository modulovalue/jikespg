cmake_minimum_required(VERSION 3.30)
project(jikespg C)

set(CMAKE_C_STANDARD 11)

include_directories(src)

add_executable(jikespg
        src/c370.h
        src/common.h
        src/ctabs.c
        src/globals.c
        src/header.h
        src/lpgact.h
        src/lpgact.i
        src/lpgdcl.h
        src/lpgdef.h
        src/lpgparse.c
        src/lpgparse.h
        src/lpgprs.h
        src/lpgsym.h
        src/lpgutil.c
        src/mkfirst.c
        src/mkred.c
        src/mkstates.c
        src/partset.c
        src/prntstat.c
        src/produce.c
        src/ptables.c
        src/reduce.h
        src/remsp.c
        src/resolve.c
        src/space.h
        src/spacetab.c
        src/tabutil.c
        src/timetab.c
        src/main.c)
