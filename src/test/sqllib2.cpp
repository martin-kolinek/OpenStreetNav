#include <boost/test/unit_test.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/regex.hpp>
#include "../sqllib/sqllib.h"

BOOST_AUTO_TEST_SUITE(sqllib2)

BOOST_AUTO_TEST_CASE(sqlcreator)
{
    boost::property_tree::ptree properties;
    std::istringstream ss("<type>intersect</type><children><child><type>simple</type><query>aaa</query></child><child><type>simple</type><query>bbb</query></child></children><ending>end</ending>");
    boost::property_tree::xml_parser::read_xml(ss, properties);
    auto creat = sqllib::SqlCreatorFactory::create(properties);
    boost::regex r("\\s*\\(?aaa\\)?\\s+intersect\\s+\\(?bbb\\)?\\s+end\\s*", boost::regex_constants::icase);
    BOOST_CHECK(boost::regex_match(creat->create_sql(), r));
}

BOOST_AUTO_TEST_CASE(kvtrans)
{
    sqllib::KeyValFilterTranslator tr("a.col1", "Test t, Abc a", "t.a=a.a", "t", std::vector<std::string> {"asdf"}, "order by a");
    boost::property_tree::ptree entries;
    std::istringstream ss("<entries><entry><add><a>asdf</a></add><elements><el><key>aaa</key><value>bbb</value></el><el><key>aaaa</key><value>bbbb</value></el></elements></entry><entry><add><a>asdfasdf</a></add><elements><el><key>ccc</key><value>ddd</value></el></elements></entry></entries>");
    boost::property_tree::xml_parser::read_xml(ss, entries);
    auto ret = tr.translate(entries);
    auto creat = sqllib::SqlCreatorFactory::create(ret);
    boost::regex r("\\s*\\(?\\s*\\(?select\\s+a.col1\\s*,\\s*asdf::asdf\\s+as\\s+a\\s+from\\s+Test t, Abc a\\s+where\\s+t.a=a.a\\s+and\\s+t.key\\s*=\\s*'aaa'\\s+and\\s+t.value\\s*=\\s*'bbb'\\s*\\)?\\s*intersect\\s*\\(?select\\s+a.col1\\s*,\\s*asdf::asdf\\s+as\\s+a\\s+from\\s+Test t, Abc a\\s+where\\s+t.a=a.a\\s+and\\s+t.key\\s*=\\s*'aaaa'\\s+and\\s+t.value\\s*=\\s*'bbbb'\\s*\\)?\\s*\\)?\\s*union\\s*\\(?\\s*\\(?select\\s+a.col1\\s*,\\s*asdfasdf::asdf\\s+as\\s+a\\s+from\\s+Test t, Abc a\\s+where\\s+t.a=a.a\\s+and\\s+t.key\\s*=\\s*'ccc'\\s+and\\s+t.value\\s*=\\s*'ddd'\\s*\\)?\\s*\\)?\\s+order\\s+by\\s+a\\s*", boost::regex_constants::icase);
    BOOST_CHECK(boost::regex_match(creat->create_sql(), r));
}

BOOST_AUTO_TEST_SUITE_END()
