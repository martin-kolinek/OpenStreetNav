/*
 * UnknownParser.h
 *
 *  Created on: Nov 10, 2011
 *      Author: martin
 */

#ifndef UNKNOWNPARSER_H_
#define UNKNOWNPARSER_H_

#include "SubParser.h"

namespace osmxml
{

class UnknownParser : public SubParser<int>
{
public:
	UnknownParser();
	virtual ~UnknownParser();
	void start(std::string const& name, xmlpp::SaxParser::AttributeList const& attrs);
	bool end(std::string const& name);
	void reset(xmlpp::SaxParser::AttributeList const& attrs);
private:
	int depth;
};

} /* namespace osmxml */
#endif /* UNKNOWNPARSER_H_ */
