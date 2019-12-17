# convert graphs from https://mast.queensu.ca/~ctardif/MYSTERY.html
# to 3color format

import sys

if len(sys.argv) < 2:
    print "need file to convert as input"
    sys.exit(1)

def addNode(v1, v2, graph):
    if v1 in graph:
        graph[v1].append(v2)
    else:
        graph[v1] = [v2]
    return graph

fname = sys.argv[1]

f = open(fname, "r")
f1 = f.readlines()
graph = dict()
for l in f1:
    line = l.split()
    if line[0] == "e":
        v1 = line[1]
        v2 = line[2]
     
            
        # create adjacency list
        graph = addNode(v1, v2, graph)
        graph = addNode(v2, v1, graph)
        
# convert the list to linkedin format
f_out = open(fname+".3color","w+")

# convert the list to linksedin format
nodes = [int(x) for x in graph]
nodes.sort()
for head in nodes:
    head = str(head)
    adjList = graph[head]
    out = head+ ":"+ ",".join(adjList)+"\n"
    print out
    f_out.write(out)
f_out.close()
