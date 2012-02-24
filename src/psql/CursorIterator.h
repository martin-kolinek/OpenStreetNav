/*
 * CursorIterator.h
 *
 *  Created on: Feb 23, 2012
 *      Author: martin
 */

#ifndef CURSORITERATOR_H_
#define CURSORITERATOR_H_

#include "Cursor.h"
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/iterator_range.hpp>

namespace psql
{

template<typename BTypes, typename RTypes>
class CursorIterator : public boost::iterator_facade<CursorIterator<BTypes, RTypes>, const typename RTypes::RowType, boost::single_pass_traversal_tag>
{
private:
    Cursor<BTypes, RTypes> * crs;
    typename std::vector<typename RTypes::RowType>::const_iterator it;
    bool is_end;

    void ensure()
    {
        if (it == crs->get_buffer().end())
        {
            crs->fetch();
            it = crs->get_buffer().begin();
            if (it == crs->get_buffer().end())
            {
                is_end = true;
            }
        }
    }
public:
    CursorIterator(Cursor<BTypes, RTypes> & crs, bool is_end)
        : crs(&crs),
          it(crs.get_buffer().begin()),
          is_end(is_end)
    {
        ensure();
    }

    const typename RTypes::RowType& dereference() const
    {
        return *it;
    }

    bool equal(const CursorIterator& other) const
    {
        return is_end == other.is_end;
    }

    void increment()
    {
        if (is_end)
            return;

        ++it;
        ensure();
    }
};

template<typename BTypes, typename RTypes>
boost::iterator_range<CursorIterator<BTypes, RTypes> > make_cursor_range(Cursor<BTypes, RTypes>& crs)
{
    return boost::make_iterator_range(CursorIterator<BTypes, RTypes>(crs, false), CursorIterator<BTypes, RTypes>(crs, true));
}

} /* namespace psql */
#endif /* CURSORITERATOR_H_ */
