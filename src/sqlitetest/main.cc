/*
 * main.cc
 *
 *  Created on: Nov 13, 2011
 *      Author: martin
 */

#include <ctime>
#include <istream>
#include <iostream>
#include <fstream>
#include "../sqlite/sqlitewrap.h"

int main(int argc, char** argv)
{
    if (argc != 3)
    {
        std::cout << "sqlitetest db sql" << std::endl;
        return 1;
    }
    std::string sql;
    std::ifstream s(argv[2]);
    std::string str;
    while (!s.eof())
    {
        s >> str;
        sql += str + " ";
    }
    sqlite::Database db(argv[1]);
    clock_t t0 = clock();
    sqlite::Statement st(sql, db);
    clock_t t1 = clock();
    std::cout << "prepare: " << (t1 - t0) / (double)CLOCKS_PER_SEC << std::endl;
    t0 = clock();
    int i = 0;
    while (!st.done())
    {
        i++;
        st.step();
    }
    t1 = clock();
    std::cout << "execute: " << i << " " << (t1 - t0) / (double)CLOCKS_PER_SEC << std::endl;
    return 0;
}


