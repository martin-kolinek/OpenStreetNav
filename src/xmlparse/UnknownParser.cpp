/*
 * UnknownParser.cpp
 *
 *  Created on: Nov 10, 2011
 *      Author: martin
 */

#include "UnknownParser.h"

namespace osmxml
{

UnknownParser::UnknownParser():
		SubParser<int>([](int&, const xmlpp::SaxParser::AttributeList& ) {}),
		depth(0)
{
}

UnknownParser::~UnknownParser()
{
}

void UnknownParser::start(const std::string &, const xmlpp::SaxParser::AttributeList &)
{
	depth++;
}

bool UnknownParser::end(const std::string &)
{
	if(--depth==0)
		return true;
	else
		return false;
}

void UnknownParser::reset(const xmlpp::SaxParser::AttributeList &)
{
	depth=1;
}

} /* namespace osmxml */
