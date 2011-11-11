/*
 * SubParser.h
 *
 *  Created on: Nov 9, 2011
 *      Author: martin
 */

#ifndef SUBPARSER_H_
#define SUBPARSER_H_

#include <functional>
#include <libxml++/libxml++.h>

namespace osmxml
{

template<typename Ret, typename... Args>
class SubParser
{};

template<typename Ret, typename Arg>
class ParserArg
{
public:
    ParserArg(std::string const& name, SubParser<Arg>& pars, std::function<void (Ret&, Arg const&)> func):
        name(name),
        pars(pars),
        func(func)
    {
    }
    std::string name;
    SubParser<Arg>& pars;
    std::function<void (Ret&, Arg const&)> func;
};

template<typename Ret, typename Arg>
ParserArg<Ret, Arg> pa(std::string const& name, SubParser<Arg>& pars, std::function<void (Ret&, Arg const&)> func)
{
    return ParserArg<Ret, Arg>(name, pars, func);
}

template<typename Ret>
class SubParser<Ret>
{
protected:
    bool inchld;
    Ret ret;
    std::function<void (Ret&, xmlpp::SaxParser::AttributeList const&)> creat;
    bool& inchild()
    {
        return inchld;
    }
public:
    SubParser(std::function<void (Ret&, xmlpp::SaxParser::AttributeList const&)> creat):
        inchld(false),
        creat(creat)
    {}
    virtual ~SubParser()
    {}
    virtual void start(std::string const&, xmlpp::SaxParser::AttributeList const&)
    {
        throw std::exception();
    }
    virtual bool end(std::string const&)
    {
        return true;
    }
    Ret& returned()
    {
        return ret;
    }
    virtual void reset(xmlpp::SaxParser::AttributeList const& attrs)
    {
        creat(ret, attrs);
    }
};

template<typename Ret, typename Head, typename... Tail>
class SubParser<Ret, Head, Tail...> : public SubParser<Ret, Tail...>
{
    typedef SubParser<Ret, Tail...> inherited;
    std::string chldname;
    bool this_chld;
    std::function<void (Ret&, Head const&)> mod;
    SubParser<Head>& chld;
protected:
    bool& inchild()
    {
        return SubParser<Ret>::inchild();
    }
    Ret& returned()
    {
        return SubParser<Ret>::returned();
    }
public:
    SubParser(std::function<void (Ret&, xmlpp::SaxParser::AttributeList const&)> create, ParserArg<Ret, Head> arg, ParserArg<Ret, Tail>... rest):
        SubParser<Ret, Tail...>(create, rest...),
        chldname(arg.name),
        this_chld(false),
        mod(arg.func),
        chld(arg.pars)
    {
    }

    void start(std::string const& name, xmlpp::SaxParser::AttributeList const& attrs)
    {
        if (inchild())
        {
            if (this_chld)
                chld.start(name, attrs);
            else
                inherited::start(name, attrs);
        }
        else
        {
            if (name == chldname || chldname == "")
            {
                this_chld = true;
                inchild() = true;
                chld.reset(attrs);
            }
            else
            {
                inherited::start(name, attrs);
            }
        }

    }

    bool end(std::string const& name)
    {
        if (!inchild())
            return true;
        if (this_chld)
        {
            if (chld.end(name))
            {
                mod(returned(), chld.returned());
                this_chld = false;
                inchild() = false;
            }
            return false;
        }
        else
            return inherited::end(name);
    }

    void reset(xmlpp::SaxParser::AttributeList const& attrs)
    {
        inherited::reset(attrs);
    }

};

template<typename Ret>
SubParser<Ret> empty_parser()
{
    return SubParser<Ret>([](Ret&, xmlpp::SaxParser::AttributeList const&) {});
}

} /* namespace osmxml */
#endif /* SUBPARSER_H_ */
