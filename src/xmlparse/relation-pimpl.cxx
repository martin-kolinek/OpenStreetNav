// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#include "relation-pimpl.hxx"

// relation_pimpl
//

void relation_pimpl::
pre ()
{
	tags.clear();
	members.clear();
}

void relation_pimpl::
tag (const std::pair<std::string, std::string>& tag)
{
	tags.insert(tag);
}

void relation_pimpl::
member (const std::pair<std::string, std::shared_ptr<osm::Element> >& member)
{
  members.insert(member);
}

void relation_pimpl::
id (long long id)
{
	_id=id;
}

osm::Relation relation_pimpl::
post_relation ()
{
	osm::Relation ret(_id);
	ret.tags=tags;
	ret.members=members;
	return ret;
}

