add_library(sqllib_str SqlCreator.cpp SqlCreatorFactory.cpp SimpleSqlCreator.cpp CompositeSqlCreator.cpp KeyValFilterTranslator.cpp)

target_link_libraries(sqllib_str ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})
