cmake_minimum_required(VERSION 3.30)
project(jikespg C)

set(CMAKE_C_STANDARD 11)

include_directories(src)

add_executable(
        src/common.h
        src/ctabs.c
        src/bitset.h
        src/globals.c
        src/lpgact.h
        src/lpgact.c
        src/lpgdcl.h
        src/lpgdef.h
        src/lpgparse.c
        src/lpgprs.h
        src/lpgsym.h
        src/lpgutil.c
        src/mkfirst.c
        src/mkred.c
        src/mkstates.c
        src/partset.c
        src/remsp.c
        src/main.c
        src/bitset.h)
