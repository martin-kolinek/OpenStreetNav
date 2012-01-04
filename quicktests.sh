#!/bin/bash

build/bin/tests --show_progress=yes --run_test=xml,OsmDBCreateTests,projection,psql_test,psql2,SqliteSimpleTests,sqllib2,util,xml
