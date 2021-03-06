// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#ifndef MEMBER_PIMPL_HXX
#define MEMBER_PIMPL_HXX

#include "member-pskel.hxx"
#include "../elements/osmelements.h"
#include <utility>
#include <memory>

class member_pimpl: public virtual member_pskel
{
  public:
  virtual void
  pre ();

  virtual void
  type (const osm::ObjectType&);

  virtual void
  ref (long long);

  virtual void
  role (const ::std::string&);

  virtual std::pair<std::string, std::shared_ptr<osm::Element> >
  post_member ();
  private:
  std::string rl;
  int64_t id;
  osm::ObjectType tp;
};

#endif // MEMBER_PIMPL_HXX
