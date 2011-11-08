template <typename ObjT>
class ObjectPiece : public TaggablePiece
{
public:
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs)
    {

    }
    ParserPiece* handle_end_element(Glib::ustring const& name);
    void add_tag(Glib::ustring const& key, Glib::ustring const& value);
private:
    HandlerPiece* parent;
};
