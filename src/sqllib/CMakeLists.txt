add_library(sqllib sqllib.cc SimpleSqlCreator.cpp CompositeSqlCreator.cpp SqlCreator.cpp)
target_link_libraries(sqllib psql)