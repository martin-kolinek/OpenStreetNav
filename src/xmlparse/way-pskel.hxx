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

#ifndef WAY_PSKEL_HXX
#define WAY_PSKEL_HXX

// Begin prologue.
//
//
// End prologue.

#include <xsd/cxx/config.hxx>

#if (XSD_INT_VERSION != 3030000L)
#error XSD runtime version mismatch
#endif

#include <xsd/cxx/pre.hxx>

// Forward declarations
//
class way_pskel;

#ifndef XSD_USE_CHAR
#define XSD_USE_CHAR
#endif

#ifndef XSD_CXX_PARSER_USE_CHAR
#define XSD_CXX_PARSER_USE_CHAR
#endif

#include "xml_schema-pskel.hxx"
#include "../elements/osmelements.h"

class tag_pskel;
class nd_pskel;
class way_pskel: public ::xml_schema::complex_content
{
  public:
  // Parser callbacks. Override them in your implementation.
  //
  // virtual void
  // pre ();

  virtual void
  tag (const std::pair<std::string, std::string>&);

  virtual void
  nd (int64_t);

  virtual void
  id (long long);

  virtual void
  uid (long long);

  virtual void
  user (const ::std::string&);

  virtual void
  timestamp (const ::xml_schema::date_time&);

  virtual void
  changeset (int);

  virtual void
  version (int);

  virtual void
  visible (bool);

  virtual osm::Way
  post_way () = 0;

  // Parser construction API.
  //
  void
  tag_parser (::tag_pskel&);

  void
  nd_parser (::nd_pskel&);

  void
  id_parser (::xml_schema::long_pskel&);

  void
  uid_parser (::xml_schema::long_pskel&);

  void
  user_parser (::xml_schema::string_pskel&);

  void
  timestamp_parser (::xml_schema::date_time_pskel&);

  void
  changeset_parser (::xml_schema::int_pskel&);

  void
  version_parser (::xml_schema::int_pskel&);

  void
  visible_parser (::xml_schema::boolean_pskel&);

  void
  parsers (::tag_pskel& /* tag */,
           ::nd_pskel& /* nd */,
           ::xml_schema::long_pskel& /* id */,
           ::xml_schema::long_pskel& /* uid */,
           ::xml_schema::string_pskel& /* user */,
           ::xml_schema::date_time_pskel& /* timestamp */,
           ::xml_schema::int_pskel& /* changeset */,
           ::xml_schema::int_pskel& /* version */,
           ::xml_schema::boolean_pskel& /* visible */);

  // Constructor.
  //
  way_pskel ();

  // Implementation.
  //
  protected:
  virtual bool
  _start_element_impl (const ::xml_schema::ro_string&,
                       const ::xml_schema::ro_string&,
                       const ::xml_schema::ro_string*);

  virtual bool
  _end_element_impl (const ::xml_schema::ro_string&,
                     const ::xml_schema::ro_string&);

  virtual bool
  _attribute_impl_phase_one (const ::xml_schema::ro_string&,
                             const ::xml_schema::ro_string&,
                             const ::xml_schema::ro_string&);


  protected:
  ::tag_pskel* tag_parser_;
  ::nd_pskel* nd_parser_;
  ::xml_schema::long_pskel* id_parser_;
  ::xml_schema::long_pskel* uid_parser_;
  ::xml_schema::string_pskel* user_parser_;
  ::xml_schema::date_time_pskel* timestamp_parser_;
  ::xml_schema::int_pskel* changeset_parser_;
  ::xml_schema::int_pskel* version_parser_;
  ::xml_schema::boolean_pskel* visible_parser_;

  protected:
  struct v_state_descr_
  {
    void (::way_pskel::*func) (
      unsigned long&,
      unsigned long&,
      const ::xml_schema::ro_string&,
      const ::xml_schema::ro_string&,
      const ::xml_schema::ro_string*,
      bool);
    unsigned long state;
    unsigned long count;
  };

  struct v_state_
  {
    v_state_descr_ data[2UL];
    unsigned long size;
  };

  v_state_ v_state_first_;
  ::xsd::cxx::parser::pod_stack v_state_stack_;

  virtual void
  _pre_e_validate ();

  virtual void
  _post_e_validate ();

  void
  choice_0 (unsigned long& state,
            unsigned long& count,
            const ::xml_schema::ro_string& ns,
            const ::xml_schema::ro_string& n,
            const ::xml_schema::ro_string* t,
            bool start);

  protected:
  struct v_state_attr_
  {
    bool id;
  };

  v_state_attr_ v_state_attr_first_;
  ::xsd::cxx::parser::pod_stack v_state_attr_stack_;

  virtual void
  _pre_a_validate ();

  virtual void
  _post_a_validate ();
};

#include <xsd/cxx/post.hxx>

// Begin epilogue.
//
//
// End epilogue.

#endif // WAY_PSKEL_HXX
