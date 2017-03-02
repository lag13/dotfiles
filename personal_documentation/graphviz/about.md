Graphviz
========

- http://www.graphviz.org/
- http://www.graphviz.org/Download..php - Where the publically let you know
  about the source code and such. You had to agree to something before getting
  access to it.
- https://github.com/ellson/graphviz/ - Source code although I think I also
  saw source code on their site so I'm not sure what the relation between that
  in this is.
- http://www.graphviz.org/pdf/dotguide.pdf - Says it's a bit outdated but most
  of it is not and has great info.

Graphviz is an open source graph visualization software. Essentially there are
two things involved:

- A language (called DOT) for describing graphs.
- Various tools for turning those descriptions into a picture. There are a
  bunch of different tools for drawing graphs in slightly different ways.

Install
-------

`brew install graphviz`

This installs a bunch of binaries each which is apparently draws graphs in a
slightly different style. The documentation on the website and the local man
pages seems to not totally agree and I'm a little confused because some of the
documentation seems inaccurate (for instance the `dot` command can be used for
drawing directed or undirected graphs) so for now here is the description from
the man pages:

- dot - filter for drawing directed graphs
- neato - filter for drawing undirected graphs
- twopi - filter for radial layouts of graphs
- circo - filter for circular layout of graphs
- fdp - filter for drawing undirected graphs
- sfdp - filter for drawing large undirected graphs
- patchwork - filter for tree maps

DOT Language Quick Intro
------------------------

- http://www.graphviz.org/content/dot-language

```
digraph test123 {
  graph [splines=polyline]
  // This is a comment
  a -> b -> c;
  /*
  multi line
  comment
  */
  a -> {x y};
  b [shape=box];
  c [label="hello\nworld",color=blue,fontsize=24,
   fontname="Palatino-Italic",fontcolor=red,style=filled];
  a -> z [label="hi", weight=100];
  x -> z [label="multi-line\nlabel"];
  edge [style=dashed,color=red];
  b -> x;
  {rank=same; b x}
}
```

```
// You can also have subgraphs inside a graph which I think just means that you
// van apply global styles to just that subgraph and sometimes the drawing tool
// will try to draw subgraphs in the same area.
digraph G {
  subgraph cluster0 {
    node [style=filled,color=white];
    style=filled;
    color=lightgrey;
    a0 -> a1 -> a2 -> a3;
    label = "process #1";
  }
  subgraph cluster1 {
    node [style=filled];
    b0 -> b1 -> b2 -> b3;
    label = "process #2";
    color=blue
  }
  start -> a0;
  start -> b0;
  a1 -> b3;
  b2 -> a3;
  a3 -> a0;
  a3 -> end;
  b3 -> end;
  start [shape=Mdiamond];
  end [shape=Msquare];
}
```

- A graph must be specified as either `graph` (the edges are drawn with `--`)
  or `digraph` (the edges are drawn with `->`). I wish for things like this
  the edge is always `--` but how it's displayed is determined by the `graph`
  or `digraph` bit.
- Semicolons are optional they can just be included for readability.
- You can prefix `strict` in front of the graph which makes it so no duplicate
  edges are possible.
- There are a lot of attributes: http://www.graphviz.org/content/attrs. One I
  looked at was "spline" which changes how edges are drawn and is visible
  above.

Command Line Quick Intro
------------------------

- http://www.graphviz.org/content/command-line-invocation

`dot -Tpng -O graph.dot`

### Options

* K<algorithm> - specifies the algorithm to use when rendering the graph,
  overriding the default from the command name. So `dot -Kneato` is the same
  as running `neato` directly. I like this, means I only need to use one
  command.
* T<format> - outputs the graph in the supported format:
  http://www.graphviz.org/content/output-formats. For example `-Tpng`.
* o<outputfile> - by default outputs to stdout.
* O - Automatically generate output file names. I believe it tries to just
  append the format specified in `T` to the file name given as an argument.
* ? - print usage information
