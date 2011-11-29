use strict;

sub trim($)
{
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}

sub begin_template
{
	open IN, $_[0];
	open OUT, ">".$_[1];
	
	while(<IN>)
	{
		if($_ =~ m/\/\/\$\$/)
		{
			last;
		}
		print OUT $_;
	}
	close IN;
	close OUT;
}

sub end_template
{
	open IN, $_[0];
	open OUT, ">>".$_[1];
	my $wr=0;
	while(<IN>)
	{
		if($wr == 1)
		{
			print OUT $_;
		}
		if($_ =~ m/\/\/\$\$/)
		{
			$wr = 1;
		}
	}
	close IN;
	close OUT;
}

begin_template("sqllib/header_templ.h", "sqllib/sqllib.h");
begin_template("sqllib/src_templ.cc", "sqllib/sqllib.cc");
begin_template("sqllib/test_templ.cc", "test/sqllib_test.cc");

open HDR, ">>sqllib/sqllib.h";
open SRC, ">>sqllib/sqllib.cc";
open TST, ">>test/sqllib_test.cc";

my $f;
my @files = <sqllib/*.sql>;
foreach $f (@files)
{
	$f =~ m/^sqllib\/(.*)\.sql$/;
	my $name = $1;
	open IN, $f;
	my $i = 0;
	my $type="";
	print TST "BOOST_AUTO_TEST_CASE(lib_$name)\n";
	print TST "{\n";
	while(<IN>)
	{
		if(/^\s*--\s*test\-depend\s+(\S+)\s+(.*)$/)
		{
			print TST "auto st$i(sqllib::get_$1(db, false));\n";
			print TST "st$i.execute($2);\n";
			$i++;
			next;
		}
		if(/^\s*--\s*type\s+(.*)$/)
		{
			print HDR "psql::Statement<$1> get_$name(psql::Database& db, bool named=false, std::string const& name=\"\");\n";
			print SRC "psql::Statement<$1> get_$name(psql::Database& db, bool named, std::string const& name)\n";
			print SRC "{\n";
			print SRC "std::string str(\"\\\n";
			$type=$1;
			next;
		}
		if(/^\s*--\s*test-param\s+(.*)$/)
		{
			print TST "auto tested_st(sqllib::get_$name(db));\n";
			print TST "tested_st.execute($1);\n";
			next;
		}
		if(/^\s*--\s*test-result\s+(.*)$/)
		{
			print TST "tested_st.get_row($1);\n";
			next;
		}
		print SRC trim($_)."\\n\\\n";
	}
	print SRC "\");\n";
	print SRC "if(named)\n";
	print SRC "    return psql::Statement<$type>(name, str, db);\n";
	print SRC "else\n";
	print SRC "    return psql::Statement<$type>(str, db);\n";
	print SRC "}\n";
	print TST "}\n";
	print HDR "\n";
	print SRC "\n";
	print TST "\n";
}

close HDR;
close SRC;
close TST;

end_template("sqllib/header_templ.h", "sqllib/sqllib.h");
end_template("sqllib/src_templ.cc", "sqllib/sqllib.cc");
end_template("sqllib/test_templ.cc", "test/sqllib_test.cc");
