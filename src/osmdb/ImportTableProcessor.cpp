/*
 * ImportTableProcessor.cpp
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#include "ImportTableProcessor.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

std::vector<ImportTableAction> const& AllImportActions::get()
{
    return vect;
}

std::vector<ImportTableAction> AllImportActions::vect = std::vector<ImportTableAction>
{
    ImportTableAction::CREATE_IMPORT_PKEY,
    ImportTableAction::CREATE_IMPORT_INDEX,
    ImportTableAction::ANALYZE,
    ImportTableAction::DELETE_NODE_TO_UPDATE,
    ImportTableAction::DELETE_WAY_TO_UPDATE,
    ImportTableAction::DELETE_RELATION_TO_UPDATE,
    ImportTableAction::DELETE_NODE_TO_DELETE,
    ImportTableAction::DELETE_WAY_TO_DELETE,
    ImportTableAction::DELETE_RELATION_TO_DELETE,
    ImportTableAction::DELETE_ORPHAN,
    ImportTableAction::DELETE_DUPLICIT_NODE,
    ImportTableAction::DELETE_DUPLICIT_WAY,
    ImportTableAction::DELETE_DUPLICIT_RELATION,
    ImportTableAction::DELETE_INCOMPLETE_WAY,
    ImportTableAction::DELETE_INCOMPLETE_RELATION,
    ImportTableAction::DELETE_IMPORT_ORPHANS,
    ImportTableAction::IMPORT_NODE,
    ImportTableAction::IMPORT_WAY,
    ImportTableAction::IMPORT_RELATION,
    ImportTableAction::IMPORT_NODE_ATTR,
    ImportTableAction::IMPORT_WAY_ATTR,
    ImportTableAction::IMPORT_REL_ATTR,
    ImportTableAction::IMPORT_WAY_NODE,
    ImportTableAction::IMPORT_MEMBER_NODE,
    ImportTableAction::IMPORT_MEMBER_WAY,
    ImportTableAction::IMPORT_MEMBER_REL,
    ImportTableAction::IMPORT_EDGES,
    ImportTableAction::CLEAR_IMPORT,
    ImportTableAction::DELETE_DUPLICIT_ATTR,
    ImportTableAction::DELETE_DUPLICIT_MEMBER,
    ImportTableAction::DELETE_DUPLICIT_WAYNODE
};

ImportTableProcessor::ImportTableProcessor(OsmDatabase& db):
    db(db)
{
    enable_all();
}

void ImportTableProcessor::process()
{
    //indexes

    auto st = sqllib::get_create_import_pkey(db.get_db());
    if (enabled[ImportTableAction::CREATE_IMPORT_PKEY])
    {
        st.execute();
        action_signal(ImportTableAction::CREATE_IMPORT_PKEY, 0);
    }
    st = sqllib::get_create_import_index(db.get_db());
    if (enabled[ImportTableAction::CREATE_IMPORT_INDEX])
    {
        st.execute();
        st = sqllib::get_create_import_index_edge(db.get_db());
        st.execute();
        action_signal(ImportTableAction::CREATE_IMPORT_INDEX, 0);
    }
    st = sqllib::get_analyze_import(db.get_db());
    if (enabled[ImportTableAction::ANALYZE])
    {
        st.execute();
        action_signal(ImportTableAction::ANALYZE, 0);
    }

    //updates
    st = sqllib::get_delete_updated_nodes(db.get_db());
    if (enabled[ImportTableAction::DELETE_NODE_TO_UPDATE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_NODE_TO_UPDATE, st.affected_rows());
    }
    st = sqllib::get_delete_updated_ways(db.get_db());
    if (enabled[ImportTableAction::DELETE_WAY_TO_UPDATE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_WAY_TO_UPDATE, st.affected_rows());
    }
    st = sqllib::get_delete_updated_relations(db.get_db());
    if (enabled[ImportTableAction::DELETE_RELATION_TO_UPDATE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_RELATION_TO_UPDATE, st.affected_rows());
    }
    //deletes
    st = sqllib::get_delete_deleted_nodes(db.get_db());
    if (enabled[ImportTableAction::DELETE_NODE_TO_DELETE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_NODE_TO_DELETE, st.affected_rows());
    }
    st = sqllib::get_delete_deleted_ways(db.get_db());
    if (enabled[ImportTableAction::DELETE_WAY_TO_DELETE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_WAY_TO_DELETE, st.affected_rows());
    }
    st = sqllib::get_delete_deleted_relations(db.get_db());
    if (enabled[ImportTableAction::DELETE_RELATION_TO_DELETE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_RELATION_TO_DELETE, st.affected_rows());
    }

    //orphans
    int64_t orp = 0;
    st = sqllib::get_delete_orphan1(db.get_db());
    if (enabled[ImportTableAction::DELETE_ORPHAN])
    {
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan2(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan3(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan4(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan5(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan6(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan7(db.get_db());
        st.execute();
        orp += st.affected_rows();
        action_signal(ImportTableAction::DELETE_ORPHAN, orp);
    }

    //duplicities
    st = sqllib::get_delete_duplicit_import_nodes(db.get_db());
    if (enabled[ImportTableAction::DELETE_DUPLICIT_NODE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_DUPLICIT_NODE, st.affected_rows());
    }
    st = sqllib::get_delete_duplicit_import_ways(db.get_db());
    if (enabled[ImportTableAction::DELETE_DUPLICIT_WAY])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_DUPLICIT_WAY, st.affected_rows());
    }
    st = sqllib::get_delete_duplicit_import_rels(db.get_db());
    if (enabled[ImportTableAction::DELETE_DUPLICIT_RELATION])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_DUPLICIT_RELATION, st.affected_rows());
    }

    //incomplete
    st = sqllib::get_delete_incomplete_ways(db.get_db());
    if (enabled[ImportTableAction::DELETE_INCOMPLETE_WAY])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_INCOMPLETE_WAY, st.affected_rows());
    }
    int64_t inc_rel = 0;
    if (enabled[ImportTableAction::DELETE_INCOMPLETE_RELATION])
    {
        st = sqllib::get_delete_incomplete_rels1(db.get_db());
        st.execute();
        inc_rel += st.affected_rows();
        st = sqllib::get_delete_incomplete_rels2(db.get_db());
        st.execute();
        inc_rel += st.affected_rows();
        st = sqllib::get_delete_incomplete_rels3(db.get_db());
        st.execute();
        inc_rel += st.affected_rows();
        action_signal(ImportTableAction::DELETE_INCOMPLETE_RELATION, inc_rel);
    }

    //orphan_import
    orp = 0;
    if (enabled[ImportTableAction::DELETE_IMPORT_ORPHANS])
    {
        st = sqllib::get_delete_orphan_import1(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import2(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import3(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import4(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import5(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import6(db.get_db());
        st.execute();
        orp += st.affected_rows();
        st = sqllib::get_delete_orphan_import7(db.get_db());
        st.execute();
        orp += st.affected_rows();
        action_signal(ImportTableAction::DELETE_IMPORT_ORPHANS, orp);
    }

    //duplicit other stuff
    int64_t dup = 0;
    if (enabled[ImportTableAction::DELETE_DUPLICIT_ATTR])
    {
        st = sqllib::get_delete_duplicit_node_attrs(db.get_db());
        st.execute();
        dup += st.affected_rows();
        st = sqllib::get_delete_duplicit_way_attrs(db.get_db());
        st.execute();
        dup += st.affected_rows();
        st = sqllib::get_delete_duplicit_rel_attrs(db.get_db());
        st.execute();
        dup += st.affected_rows();
        action_signal(ImportTableAction::DELETE_DUPLICIT_ATTR, dup);
    }

    st = sqllib::get_delete_duplicit_waynodes(db.get_db());
    if (enabled[ImportTableAction::DELETE_DUPLICIT_WAYNODE])
    {
        st.execute();
        action_signal(ImportTableAction::DELETE_DUPLICIT_WAYNODE, st.affected_rows());
    }

    dup = 0;
    st = sqllib::get_delete_duplicit_node_members(db.get_db());
    if (enabled[ImportTableAction::DELETE_DUPLICIT_MEMBER])
    {
        st.execute();
        dup += st.affected_rows();
        st = sqllib::get_delete_duplicit_way_members(db.get_db());
        st.execute();
        dup += st.affected_rows();
        st = sqllib::get_delete_duplicit_rel_members(db.get_db());
        st.execute();
        dup += st.affected_rows();
        action_signal(ImportTableAction::DELETE_DUPLICIT_MEMBER, dup);
    }

    if (proceed_signal())
    {
        //import
        st = sqllib::get_do_import1(db.get_db());
        if (enabled[ImportTableAction::IMPORT_NODE])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_NODE, st.affected_rows());
        }
        st = sqllib::get_do_import2(db.get_db());
        if (enabled[ImportTableAction::IMPORT_WAY])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_WAY, st.affected_rows());
        }
        st = sqllib::get_do_import3(db.get_db());
        if (enabled[ImportTableAction::IMPORT_RELATION])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_RELATION, st.affected_rows());
        }
        st = sqllib::get_do_import4(db.get_db());
        if (enabled[ImportTableAction::IMPORT_NODE_ATTR])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_NODE_ATTR, st.affected_rows());
        }
        st = sqllib::get_do_import5(db.get_db());
        if (enabled[ImportTableAction::IMPORT_WAY_ATTR])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_WAY_ATTR, st.affected_rows());
        }
        st = sqllib::get_do_import6(db.get_db());
        if (enabled[ImportTableAction::IMPORT_REL_ATTR])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_REL_ATTR, st.affected_rows());
        }
        st = sqllib::get_do_import7(db.get_db());
        if (enabled[ImportTableAction::IMPORT_WAY_NODE])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_WAY_NODE, st.affected_rows());
        }
        st = sqllib::get_do_import8(db.get_db());
        if (enabled[ImportTableAction::IMPORT_MEMBER_NODE])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_MEMBER_NODE, st.affected_rows());
        }
        st = sqllib::get_do_import9(db.get_db());
        if (enabled[ImportTableAction::IMPORT_MEMBER_WAY])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_MEMBER_WAY, st.affected_rows());
        }
        st = sqllib::get_do_import10(db.get_db());
        if (enabled[ImportTableAction::IMPORT_MEMBER_REL])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_MEMBER_REL, st.affected_rows());
        }
        st = sqllib::get_do_import11(db.get_db());
        if (enabled[ImportTableAction::IMPORT_EDGES])
        {
            st.execute();
            action_signal(ImportTableAction::IMPORT_EDGES, st.affected_rows());
        }

        if (enabled[ImportTableAction::CLEAR_IMPORT])
        {
            st = sqllib::get_drop_import_pkey(db.get_db());
            st.execute();
            st = sqllib::get_drop_import_index(db.get_db());
            st.execute();
            st = sqllib::get_drop_import_index_edge(db.get_db());
            st.execute();
            st = sqllib::get_clear_import_table(db.get_db());
            st.execute();
            action_signal(ImportTableAction::CLEAR_IMPORT, st.affected_rows());
        }
    }
}

void ImportTableProcessor::set(ImportTableAction act, bool val)
{
    enabled[act] = val;
}

void ImportTableProcessor::enable(ImportTableAction act)
{
    set(act, true);
}

void ImportTableProcessor::disable(ImportTableAction act)
{
    set(act, false);
}

void ImportTableProcessor::set_all(bool val)
{
    for (unsigned int i = 0; i < AllImportActions::get().size(); ++i)
    {
        enabled[AllImportActions::get()[i]] = val;
    }
}

void ImportTableProcessor::enable_all()
{
    set_all(true);
}

void ImportTableProcessor::disable_all()
{
    set_all(false);
}

ImportTableProcessor::~ImportTableProcessor()
{
}

} /* namespace osmdb */
