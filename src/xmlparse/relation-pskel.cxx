// Copyright (C) 2005-2010 Code Synthesis Tools CC
//
// This program was generated by CodeSynthesis XSD, an XML Schema to
// C++ data binding compiler.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
//
// In addition, as a special exception, Code Synthesis Tools CC gives
// permission to link this program with the Xerces-C++ library (or with
// modified versions of Xerces-C++ that use the same license as Xerces-C++),
// and distribute linked combinations including the two. You must obey
// the GNU General Public License version 2 in all respects for all of
// the code used other than Xerces-C++. If you modify this copy of the
// program, you may extend this exception to your version of the program,
// but you are not obligated to do so. If you do not wish to do so, delete
// this exception statement from your version.
//
// Furthermore, Code Synthesis Tools CC makes a special exception for
// the Free/Libre and Open Source Software (FLOSS) which is described
// in the accompanying FLOSSE file.
//

// Begin prologue.
//
//
// End prologue.

#include "relation-pskel.hxx"

#include "tag-pskel.hxx"

#include "member-pskel.hxx"

// relation_pskel
//

void relation_pskel::
tag_parser (::tag_pskel& p)
{
  this->tag_parser_ = &p;
}

void relation_pskel::
member_parser (::member_pskel& p)
{
  this->member_parser_ = &p;
}

void relation_pskel::
id_parser (::xml_schema::long_pskel& p)
{
  this->id_parser_ = &p;
}

void relation_pskel::
uid_parser (::xml_schema::long_pskel& p)
{
  this->uid_parser_ = &p;
}

void relation_pskel::
user_parser (::xml_schema::string_pskel& p)
{
  this->user_parser_ = &p;
}

void relation_pskel::
timestamp_parser (::xml_schema::date_time_pskel& p)
{
  this->timestamp_parser_ = &p;
}

void relation_pskel::
changeset_parser (::xml_schema::int_pskel& p)
{
  this->changeset_parser_ = &p;
}

void relation_pskel::
version_parser (::xml_schema::int_pskel& p)
{
  this->version_parser_ = &p;
}

void relation_pskel::
visible_parser (::xml_schema::boolean_pskel& p)
{
  this->visible_parser_ = &p;
}

void relation_pskel::
parsers (::tag_pskel& tag,
         ::member_pskel& member,
         ::xml_schema::long_pskel& id,
         ::xml_schema::long_pskel& uid,
         ::xml_schema::string_pskel& user,
         ::xml_schema::date_time_pskel& timestamp,
         ::xml_schema::int_pskel& changeset,
         ::xml_schema::int_pskel& version,
         ::xml_schema::boolean_pskel& visible)
{
  this->tag_parser_ = &tag;
  this->member_parser_ = &member;
  this->id_parser_ = &id;
  this->uid_parser_ = &uid;
  this->user_parser_ = &user;
  this->timestamp_parser_ = &timestamp;
  this->changeset_parser_ = &changeset;
  this->version_parser_ = &version;
  this->visible_parser_ = &visible;
}

relation_pskel::
relation_pskel ()
: tag_parser_ (0),
  member_parser_ (0),
  id_parser_ (0),
  uid_parser_ (0),
  user_parser_ (0),
  timestamp_parser_ (0),
  changeset_parser_ (0),
  version_parser_ (0),
  visible_parser_ (0),
  v_state_stack_ (sizeof (v_state_), &v_state_first_),
  v_state_attr_stack_ (sizeof (v_state_attr_), &v_state_attr_first_)
{
}

// relation_pskel
//

void relation_pskel::
tag (const std::pair<std::string, std::string>&)
{
}

void relation_pskel::
member (const std::pair<std::string, std::shared_ptr<osm::Element> >&)
{
}

void relation_pskel::
id (long long)
{
}

void relation_pskel::
uid (long long)
{
}

void relation_pskel::
user (const ::std::string&)
{
}

void relation_pskel::
timestamp (const ::xml_schema::date_time&)
{
}

void relation_pskel::
changeset (int)
{
}

void relation_pskel::
version (int)
{
}

void relation_pskel::
visible (bool)
{
}

#include <cassert>

// Element validation and dispatch functions for relation_pskel.
//
bool relation_pskel::
_start_element_impl (const ::xml_schema::ro_string& ns,
                     const ::xml_schema::ro_string& n,
                     const ::xml_schema::ro_string* t)
{
  XSD_UNUSED (t);

  v_state_& vs = *static_cast< v_state_* > (this->v_state_stack_.top ());
  v_state_descr_* vd = vs.data + (vs.size - 1);

  if (vd->func == 0 && vd->state == 0)
  {
    if (this->::xml_schema::complex_content::_start_element_impl (ns, n, t))
      return true;
    else
      vd->state = 1;
  }

  while (vd->func != 0)
  {
    (this->*vd->func) (vd->state, vd->count, ns, n, t, true);

    vd = vs.data + (vs.size - 1);

    if (vd->state == ~0UL)
      vd = vs.data + (--vs.size - 1);
    else
      break;
  }

  if (vd->func == 0)
  {
    if (vd->state != ~0UL)
    {
      unsigned long s = ~0UL;

      if (n == "tag" && ns.empty ())
        s = 0UL;
      else if (n == "member" && ns.empty ())
        s = 1UL;

      if (s != ~0UL)
      {
        vd->count++;

        vd = vs.data + vs.size++;
        vd->func = &relation_pskel::choice_0;
        vd->state = s;
        vd->count = 0;

        this->choice_0 (vd->state, vd->count, ns, n, t, true);
      }
      else
      {
        if (vd->count < 1UL)
          this->_expected_element (
            "", "tag",
            ns, n);
        return false;
      }
    }
    else
      return false;
  }

  return true;
}

bool relation_pskel::
_end_element_impl (const ::xml_schema::ro_string& ns,
                   const ::xml_schema::ro_string& n)
{
  v_state_& vs = *static_cast< v_state_* > (this->v_state_stack_.top ());
  v_state_descr_& vd = vs.data[vs.size - 1];

  if (vd.func == 0 && vd.state == 0)
  {
    if (!::xml_schema::complex_content::_end_element_impl (ns, n))
      assert (false);
    return true;
  }

  assert (vd.func != 0);
  (this->*vd.func) (vd.state, vd.count, ns, n, 0, false);

  if (vd.state == ~0UL)
    vs.size--;

  return true;
}

void relation_pskel::
_pre_e_validate ()
{
  this->v_state_stack_.push ();
  static_cast< v_state_* > (this->v_state_stack_.top ())->size = 0;

  v_state_& vs = *static_cast< v_state_* > (this->v_state_stack_.top ());
  v_state_descr_& vd = vs.data[vs.size++];

  vd.func = 0;
  vd.state = 0;
  vd.count = 0;
}

void relation_pskel::
_post_e_validate ()
{
  v_state_& vs = *static_cast< v_state_* > (this->v_state_stack_.top ());
  v_state_descr_* vd = vs.data + (vs.size - 1);

  ::xml_schema::ro_string empty;
  while (vd->func != 0)
  {
    (this->*vd->func) (vd->state, vd->count, empty, empty, 0, true);
    assert (vd->state == ~0UL);
    vd = vs.data + (--vs.size - 1);
  }

  if (vd->count < 1UL)
    this->_expected_element (
      "", "tag");

  this->v_state_stack_.pop ();
}

void relation_pskel::
choice_0 (unsigned long& state,
          unsigned long& count,
          const ::xml_schema::ro_string& ns,
          const ::xml_schema::ro_string& n,
          const ::xml_schema::ro_string* t,
          bool start)
{
  XSD_UNUSED (count);
  XSD_UNUSED (ns);
  XSD_UNUSED (n);
  XSD_UNUSED (t);

  switch (state)
  {
    case 0UL:
    {
      if (start)
      {
        this->::xml_schema::complex_content::context_.top ().parser_ = this->tag_parser_;

        if (this->tag_parser_)
          this->tag_parser_->pre ();
      }
      else
      {
        if (this->tag_parser_)
        {
          const std::pair<std::string, std::string>& tmp (this->tag_parser_->post_tag ());
          this->tag (tmp);
        }

        state = ~0UL;
      }

      break;
    }
    case 1UL:
    {
      if (start)
      {
        this->::xml_schema::complex_content::context_.top ().parser_ = this->member_parser_;

        if (this->member_parser_)
          this->member_parser_->pre ();
      }
      else
      {
        if (this->member_parser_)
        {
          const std::pair<std::string, std::shared_ptr<osm::Element> >& tmp (this->member_parser_->post_member ());
          this->member (tmp);
        }

        state = ~0UL;
      }

      break;
    }
  }
}

// Attribute validation and dispatch functions for relation_pskel.
//
bool relation_pskel::
_attribute_impl_phase_one (const ::xml_schema::ro_string& ns,
                           const ::xml_schema::ro_string& n,
                           const ::xml_schema::ro_string& s)
{
  if (n == "id" && ns.empty ())
  {
    if (this->id_parser_)
    {
      this->id_parser_->pre ();
      this->id_parser_->_pre_impl ();
      this->id_parser_->_characters (s);
      this->id_parser_->_post_impl ();
      long long tmp (this->id_parser_->post_long ());
      this->id (tmp);
    }

    static_cast< v_state_attr_* > (this->v_state_attr_stack_.top ())->id = true;
    return true;
  }

  if (n == "uid" && ns.empty ())
  {
    if (this->uid_parser_)
    {
      this->uid_parser_->pre ();
      this->uid_parser_->_pre_impl ();
      this->uid_parser_->_characters (s);
      this->uid_parser_->_post_impl ();
      long long tmp (this->uid_parser_->post_long ());
      this->uid (tmp);
    }

    return true;
  }

  if (n == "user" && ns.empty ())
  {
    if (this->user_parser_)
    {
      this->user_parser_->pre ();
      this->user_parser_->_pre_impl ();
      this->user_parser_->_characters (s);
      this->user_parser_->_post_impl ();
      const ::std::string& tmp (this->user_parser_->post_string ());
      this->user (tmp);
    }

    return true;
  }

  if (n == "timestamp" && ns.empty ())
  {
    if (this->timestamp_parser_)
    {
      this->timestamp_parser_->pre ();
      this->timestamp_parser_->_pre_impl ();
      this->timestamp_parser_->_characters (s);
      this->timestamp_parser_->_post_impl ();
      const ::xml_schema::date_time& tmp (this->timestamp_parser_->post_date_time ());
      this->timestamp (tmp);
    }

    return true;
  }

  if (n == "changeset" && ns.empty ())
  {
    if (this->changeset_parser_)
    {
      this->changeset_parser_->pre ();
      this->changeset_parser_->_pre_impl ();
      this->changeset_parser_->_characters (s);
      this->changeset_parser_->_post_impl ();
      int tmp (this->changeset_parser_->post_int ());
      this->changeset (tmp);
    }

    return true;
  }

  if (n == "version" && ns.empty ())
  {
    if (this->version_parser_)
    {
      this->version_parser_->pre ();
      this->version_parser_->_pre_impl ();
      this->version_parser_->_characters (s);
      this->version_parser_->_post_impl ();
      int tmp (this->version_parser_->post_int ());
      this->version (tmp);
    }

    return true;
  }

  if (n == "visible" && ns.empty ())
  {
    if (this->visible_parser_)
    {
      this->visible_parser_->pre ();
      this->visible_parser_->_pre_impl ();
      this->visible_parser_->_characters (s);
      this->visible_parser_->_post_impl ();
      bool tmp (this->visible_parser_->post_boolean ());
      this->visible (tmp);
    }

    return true;
  }

  return false;
}

void relation_pskel::
_pre_a_validate ()
{
  this->v_state_attr_stack_.push ();
  v_state_attr_& as = *static_cast< v_state_attr_* > (this->v_state_attr_stack_.top ());

  as.id = false;
}

void relation_pskel::
_post_a_validate ()
{
  v_state_attr_& as = *static_cast< v_state_attr_* > (this->v_state_attr_stack_.top ());

  if (!as.id)
    this->_expected_attribute (
      "", "id");

  this->v_state_attr_stack_.pop ();
}

// Begin epilogue.
//
//
// End epilogue.

