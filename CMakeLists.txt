cmake_minimum_required(VERSION 3.20)

project(cinder C)

if (WIN32)
    message("Building on Windows...")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c17 -O2 -static -Wall -Wextra -Wshadow -Wundef -Wwrite-strings -Wredundant-decls -Wdisabled-optimization -Wdouble-promotion -Wmissing-declarations -Wconversion -Wstrict-overflow=2 -Wfatal-errors")
elseif(UNIX)
    message("Building on Unix...")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c17 -O2 -Wall -Wextra -Wshadow -Wundef -Wwrite-strings -Wredundant-decls -Wdisabled-optimization -Wdouble-promotion -Wmissing-declarations -Wconversion -Wstrict-overflow=2 -Wfatal-errors")
endif()

add_executable(cinder src/cinder.c)