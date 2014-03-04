import re
import networkx

def parseEdgeLabel(edge_label):
  begin_idx = len("Line ")
  end_idx = edge_label.find(':')
  assert end_idx != -1, "Error when processing edge label."
  line_no = int(edge_label[begin_idx:end_idx])
  statement = edge_label[end_idx+2:]
  return line_no, statement 

def parseNodeLabel(node_label):
  tup = node_label.split("\\n")
  arg_nid, cfa_nid = tup[0].split(" @ ")
  scope_name = tup[1]
  m = re.findall("\d+", tup[2])
  abs_id = int(m[0])
  return cfa_nid, scope_name, abs_id 

class ARGHandler:
  def __init__(self, arg_file_name):
    self._arg = networkx.read_dot(arg_file_name)

  def traverseEdgePostOrder(self):
    root = "1"
    G = self._arg

    nodes = [root]
    visited = set()
    for start in nodes:
      if start in visited:
        continue
      visited.add(start)
      stack = [(start, iter(G[start]))]
      while stack:
        parent,children = stack[-1]
        try:
          child = next(children)
          edge_label = G.edge[parent][child][0]["label"]
          if edge_label!="covered by" and child not in visited:
            visited.add(child)
            stack.append((child, iter(G[child])))
        except StopIteration: # Exception raised by next()
          stack.pop()
          if stack:
            child = parent
            parent = stack[-1][0]
            edge_label = G.edge[parent][child][0]["label"]
            line_no, stmt = parseEdgeLabel(edge_label)
            yield parent, child, line_no
  def getNodeABSId(self, node_name):
    label = self._arg.node[node_name]["label"]
    return (parseNodeLabel(label))[2]

