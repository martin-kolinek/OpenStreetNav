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

#ifndef TAG_PSKEL_HXX
#define TAG_PSKEL_HXX

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
class tag_pskel;

#ifndef XSD_USE_CHAR
#define XSD_USE_CHAR
#endif

#ifndef XSD_CXX_PARSER_USE_CHAR
#define XSD_CXX_PARSER_USE_CHAR
#endif

#include "xml_schema-pskel.hxx"

class tag_pskel: public ::xml_schema::complex_content
{
  public:
  // Parser callbacks. Override them in your implementation.
  //
  // virtual void
  // pre ();

  virtual void
  k (const ::std::string&);

  virtual void
  v (const ::std::string&);

  virtual std::pair<std::string, std::string>
  post_tag () = 0;

  // Parser construction API.
  //
  void
  k_parser (::xml_schema::string_pskel&);

  void
  v_parser (::xml_schema::string_pskel&);

  void
  parsers (::xml_schema::string_pskel& /* k */,
           ::xml_schema::string_pskel& /* v */);

  // Constructor.
  //
  tag_pskel ();

  // Implementation.
  //
  protected:
  virtual bool
  _attribute_impl_phase_one (const ::xml_schema::ro_string&,
                             const ::xml_schema::ro_string&,
                             const ::xml_schema::ro_string&);


  protected:
  ::xml_schema::string_pskel* k_parser_;
  ::xml_schema::string_pskel* v_parser_;

  protected:
  struct v_state_attr_
  {
    bool k;
    bool v;
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

#endif // TAG_PSKEL_HXX