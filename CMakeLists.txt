cmake_minimum_required(VERSION 3.0)

enable_language(C)
project(efriso VERSION 0.1.0 DESCRIPTION "libefriso bindings for emacs")

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "shared" FORCE)
set(BUILD_CLAR OFF CACHE BOOL "clar" FORCE)

if (APPLE)
  if (NOT DEFINED ENV{FRISO_PATH})
    set( ENV{FRISO_PATH} "./friso")
    message(WARNING "set PATH:$ENV{FRISO_PATH}/src")
  endif()
  link_directories("$ENV{FRISO_PATH}/src")
  include_directories("$ENV{FRISO_PATH}/src")
elseif (UNIX)
  find_library(friso libfriso.so REQURIED)
endif()

add_library(efriso SHARED
  src/interface.c
  src/libefriso.c
  src/init.c)
set_target_properties(efriso PROPERTIES C_STANDARD 99)

# target_link_libraries(efriso friso)
target_link_libraries(efriso libfriso.a)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
if(APPLE)
  set_target_properties(efriso PROPERTIES SUFFIX .so)
endif(APPLE)

if(WIN32)
  set_target_properties(efriso PROPERTIES PREFIX lib)
endif(WIN32)

if ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_C_FLAGS_DEBUG "-fuse-ld=gold -fsanitize=address,leak,undefined -fno-omit-frame-pointer -pg -g")
endif()
