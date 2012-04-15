# - Find PostgreSQLServer
# Find the PostgreSQL includes and client library
# This module defines
#  POSTGRESQL_INCLUDE_DIR, where to find postgres.h
#  LIBPQ_INCLUDE_DIR, where to find libpq-fe.h
#  POSTGRESQL_LIBRARIES, the libraries needed to use POSTGRESQL.
#  POSTGRESQL_FOUND, If false, do not try to use PostgreSQL.
#  POSTGRESQL_EXECUTABLE
#
# Copyright (c) 2006, Jaroslaw Staniek, <js@iidea.pl>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# Add the postgresql and mysql include paths here

if(LIBPQ_INCLUDE_DIR AND POSTGRESQL_INCLUDE_DIR AND POSTGRESQL_LIBRARIES AND POSTGRESQL_EXECUTABLE)
   set(POSTGRESQL_FOUND TRUE)
else(LIBPQ_INCLUDE_DIR AND POSTGRESQL_INCLUDE_DIR AND POSTGRESQL_LIBRARIES AND POSTGRESQL_EXECUTABLE)

 FIND_PROGRAM(POSTGRESQL_EXECUTABLE postgres)
 MESSAGE(STATUS "POSTGRESQL_EXECUTABLE is " ${POSTGRESQL_EXECUTABLE})


 FIND_PATH(POSTGRESQL_INCLUDE_DIR postgres.h
      /usr/include/server
      /usr/include/pgsql/server
      /usr/local/include/pgsql/server
      /usr/include/postgresql/server
      /usr/include/postgresql/*/server
      /usr/local/include/postgresql/server
      /usr/local/include/postgresql/*/server
      $ENV{ProgramFiles}/PostgreSQL/*/include/server
      $ENV{SystemDrive}/PostgreSQL/*/include/server
      /usr/include
      /usr/include/pgsql
      /usr/local/include/pgsql
      /usr/include/postgresql/*
      /usr/local/include/postgresql
      /usr/local/include/postgresql/*
      $ENV{ProgramFiles}/PostgreSQL/*/include
      $ENV{SystemDrive}/PostgreSQL/*/include
      )
  FIND_PATH(LIBPQ_INCLUDE_DIR libpq-fe.h
      /usr/include/server
      /usr/include/pgsql/server
      /usr/local/include/pgsql/server
      /usr/include/postgresql/server
      /usr/include/postgresql/*/server
      /usr/local/include/postgresql/server
      /usr/local/include/postgresql/*/server
      $ENV{ProgramFiles}/PostgreSQL/*/include/server
      $ENV{SystemDrive}/PostgreSQL/*/include/server
      /usr/include
      /usr/include/pgsql
      /usr/local/include/pgsql
      /usr/include/postgresql/*
      /usr/local/include/postgresql
      /usr/local/include/postgresql/*
      $ENV{ProgramFiles}/PostgreSQL/*/include
      $ENV{SystemDrive}/PostgreSQL/*/include
      )

  find_library(POSTGRESQL_LIBRARIES NAMES pq libpq
     PATHS
     /usr/lib
     /usr/local/lib
     /usr/lib/postgresql
     /usr/lib64
     /usr/local/lib64
     /usr/lib64/postgresql
     $ENV{ProgramFiles}/PostgreSQL/*/lib/ms
     $ENV{SystemDrive}/PostgreSQL/*/lib/ms
     )
      
  if(POSTGRESQL_INCLUDE_DIR AND LIBPQ_INCLUDE_DIR AND POSTGRESQL_LIBRARIES)
    set(POSTGRESQL_FOUND TRUE)
    message(STATUS "Found PostgreSQL: ${POSTGRESQL_INCLUDE_DIR}, ${LIBPQ_INCLUDE_DIR}, ${POSTGRESQL_LIBRARIES}")
    INCLUDE_DIRECTORIES(${POSTGRESQL_INCLUDE_DIR})
  else(POSTGRESQL_INCLUDE_DIR AND LIBPQ_INCLUDE_DIR AND POSTGRESQL_LIBRARIES)
    set(POSTGRESQL_FOUND FALSE)
    message(STATUS "PostgreSQL not found.")
  endif(POSTGRESQL_INCLUDE_DIR AND LIBPQ_INCLUDE_DIR AND POSTGRESQL_LIBRARIES)

  mark_as_advanced(POSTGRESQL_INCLUDE_DIR POSTGRESQL_LIBRARIES)

endif(LIBPQ_INCLUDE_DIR AND POSTGRESQL_INCLUDE_DIR AND POSTGRESQL_LIBRARIES AND POSTGRESQL_EXECUTABLE)
