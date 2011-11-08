/*
 * TaggablePiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef TAGGABLEPIECE_H_
#define TAGGABLEPIECE_H_

#include "ParserPiece.h"

namespace osmxml
{

class TaggablePiece : public ParserPiece
{
public:
    virtual void add_tag(Glib::ustring const& key, Glib::ustring const& value) = 0;
    virtual ~TaggablePiece();
};

} /* namespace osmxml */
#endif /* TAGGABLEPIECE_H_ */
