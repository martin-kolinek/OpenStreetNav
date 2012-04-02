#!/bin/bash
#script which generates precomputed data for schema given as first argument
SCHBASE=$1
#way reduction for maps
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red6 -I share/map6.txt -l 0.7
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red5 -l 1.5 -I share/map5.txt
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red4 -l 3 -I share/map4.txt
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red3 -l 6 -I share/map2.txt
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red2 -l 12 -I share/map2.txt
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red1 -l 24 -I share/map1.txt
#edges for maps
./bin/edgecreate -x share/toshow/to_show_edges/1.xml -i ${SCHBASE}_red1,${SCHBASE} -o zoom1 -c
./bin/edgecreate -x share/toshow/to_show_edges/2.xml -i ${SCHBASE}_red2,${SCHBASE} -o zoom2 -c
./bin/edgecreate -x share/toshow/to_show_edges/2.xml -i ${SCHBASE}_red3,${SCHBASE} -o zoom3 -c
./bin/edgecreate -x share/toshow/to_show_edges/4.xml -i ${SCHBASE}_red4,${SCHBASE} -o zoom4 -c
./bin/edgecreate -x share/toshow/to_show_edges/5.xml -i ${SCHBASE}_red5,${SCHBASE} -o zoom5 -c
./bin/edgecreate -x share/toshow/to_show_edges/6.xml -i ${SCHBASE}_red6,${SCHBASE} -o zoom6 -c
./bin/edgecreate -x share/toshow/to_show_edges/8.xml -i ${SCHBASE} -o zoom8 -c
./bin/edgecreate -x share/toshow/to_show_edges/10.xml -i ${SCHBASE} -o zoom10 -c
#wayreduction for roads
./bin/wayreduction -b ${SCHBASE} -s ${SCHBASE}_red -I share/way.txt
#roads
./bin/roaddbcreate -f ${SCHBASE} -r ${SCHBASE}_red -o roads -n -I share/way.txt
