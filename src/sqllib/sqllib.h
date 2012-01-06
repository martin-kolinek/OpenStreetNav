#ifndef SQLLIB_CUSTOM_H_
#define SQLLIB_CUSTOM_H_

#include "strings/sqllib_str.h"
#include "../psql/psql.h"
#include <sqllib_gen.h>
#include <boost/property_tree/ptree.hpp>

namespace sqllib
{

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, double, double, double, double, double, int, int> > get_toshow_edges_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named = false, std::string name = "");

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> > get_selected_edges_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named = false, std::string name = "");

}

#endif
