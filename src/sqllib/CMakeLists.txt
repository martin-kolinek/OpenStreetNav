add_subdirectory(strings)

add_library(sqllib sqllib.cc sqllib_custom.cpp)
target_link_libraries(sqllib psql sqllib_str)