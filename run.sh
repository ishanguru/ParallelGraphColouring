NPRO=$1
ALGO=$2
METHOD=$3
INPUT=$4
OUTPUT=$5
NCOLORS=$6
./graph_colouring +RTS -N$NPRO -s -l -RTS $INPUT $NCOLORS $ALGO $METHOD $OUTPUT
mv graph_coloring.eventlog graph_coloring_N$NPRO_$ALGO_$METHOD_$INPUT_$NCOLORS.eventlog
