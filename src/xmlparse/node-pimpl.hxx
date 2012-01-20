// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#ifndef NODE_PIMPL_HXX
#define NODE_PIMPL_HXX

#include <map>
#include "../elements/osmelements.h"
#include "node-pskel.hxx"

class node_pimpl: public virtual node_pskel
{
  public:
  virtual void
  pre ();

  virtual void
  tag (const std::pair<std::string, std::string>&);

  virtual void
  id (long long);

  virtual void
  lat (double);

  virtual void
  lon (double);

  virtual osm::Node
  post_node ();
  private:
  double lt, ln;
  int64_t _id;
  std::set<osm::Tag> tags;
};

#endif // NODE_PIMPL_HXX
