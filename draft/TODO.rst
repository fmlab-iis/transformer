From reviewr 1
==============

I have made a note of various minor issues; see below.

Typos
-----
- p1
  + nondeterminsitic -> nondeterministic

- p2
  + 140 thousands -> 140 thousand
  + codes -> code (4 times)
  + nodeterministic -> nondeterministic

- p3
  + labled -> labeled

- p5
  + satisified -> satisfied
  + unwind every CFGs -> unwind every CFG

- p6
  + recursive-free -> recursion-free

- p7
  + "H^fby" -> "H^f by"
- p8
  + "every function calls in G" -> "every function call in G"

- p9
  + "4.3 Computing Summary" -> "4.3 Computing Summaries"
    (or "4.3 Computing a Summary")
  + "pairs of locations" -> "pair of locations"

- p10
  + Stregthening -> Strengthening

- p11
  + "4.4 Checking Summary" -> "4.4 Checking Summaries"
    (or "4.4 Checking a Summary")
  + In the definition of \hat{cmd}(l,l'), where it says "assume S[g]",
    the typeface for S is wrong.

- p13
  + "a solid evidence" -> "solid evidence"
  + "not merely extends -> "not only extends"

-p15
  + Particularly -> In particular
  + "Ufo: A framework" -> "UFO: A framework"

Others
------
- p2
  + Could you briefly explain what Vars' is for? This would have helped me
    understand the definition of inductive invariants (top of p4). My 
    understanding: x' represents the "new" value of x after execution of a
    command.
  + In the last three reductions for Command (bottom of p2), why do you use
    "q"? Why not continue using "p", since that is the default symbol for
    Expressions?

- p3
  + You say "Let t_i denote the i-th element in the sequence \bar{t}", but I
    *think* I'm correct in saying that you don't actually go on to use this
    notation.
  + "The superscript in Gf denotes the CFG corresponds to the function f."
    -> delete this, it's already obvious.
  + "We assume that there are no global variables because they can be 
    simulated by allowing simultaneous assignment to return variables." -> I 
    thought this was rather neat. It could warrant a little more explanation,
    i.e. explaining that the global variables are passed in to every function
    as formal parameters, and then passed out (possibly modified) in the
    function's return values. If this is a well-known trick, perhaps you could
    provide a reference, and if not, perhaps you could emphasise its novelty.
  + McCarthy's 91 program could use a citation.

- p4
  + I wouldn't call \Pi an "inductive invariant". To me, an invariant is a
    single formula, e.g. a loop invariant. But \Pi is a *set* of formulas.
    So I would consider calling \Pi an "inductive invariant family" (or maybe
    just an "invariant family").
  + "weak correctness" -- I would say "partial correctness" is the standard term
    here.
  + "Proposition 1" -- this feels more like a Definition.
  + I'm confused by "Indeed, most inductive program analyzers simply report
    false positives when inductive invariants are too coarse." By "coarse"
    do you mean "weak"? And if so, is this not a "false negative", because
    the assertion is spuriously *failing*?
  + "such as CPAChecker, Blast, UFO, Astree, etc" -- these need citations.

- p5
  + In Algorithm 1, I would omit the semicolons at the end of each line --
    they hamper readability.

- p6
  + recursive-free -> recursion-free
  + Where does the summary S[mc91] = "not(m>=0)" come from? It seems completely
    random to me. Besides, it doesn't even pass the CheckSummary in Fig 3 -- if
    you take the path s->1->5->e, then the assertion fails. You should explain
    where "not(m>=0)" comes from, and explain that BasicAnalyzer will fail when
    checking Fig 3.

- p8
  + There is no reference to Figure 5.
  + Second paragraph of Section 4.1 begins with an incomplete sentence
    ("Given...").

- p9
  + There is no reference to Figure 6
    (and its caption should be "Under-approximation")
  + In Algorithm 2, the test condition of the if-statement is phrased negatively.
    You can save a few words by changing "does not contain" to "contains" and
    swapping the then- and else-branches. Plus, this brings you into line with
    the order you discuss the two branches on page 10.

- p10
  + There is no reference to Figure 7
    (and its caption should be "Updating a Summary").
  + I suggest you write "Postcondition weakening" and "Precondition
    strengthening" rather than just "weakening" and "strengthening".
    It looks a bit odd otherwise.

- p11
  + There is no reference to Figure 8
    (and its caption should be "Checking a Summary").

- p12
  + Emphasise that you're running *all* the benchmarks in the SV-COMP recursive
    category. E.g. change "experiments with the benchmarks" to "experiments
    with all the benchmarks", or (on p13) "category include 16" to "category
    comprise 16".

- p14
  + You should consider the precision of the numbers in Table 2 -- is it really
    necessary to report timings to millisecond precision? Maybe 2 significant
    figures would be more appropriate here. Also, your timing columns should
    mention the units (seconds) explicitly.

- p15
  + No need to cite both [13] and [14], since [14] is just the extended journal
    version of [13]. In fact, it's not clear why you're citing either of them,
    since they're about verifying concurrent programs, and your work is
    entirely sequential.
  + "hopelessly incomplete" is a bit too informal.
  + You mention the "Hoare Logic proof rule for recursive calls" several times
    -- can you state the rule in the paper?
  + The sentence "Our work is inspired by Whale" is weak. Inspired in what way?
  + You mention problems with the Whale tool -- seg faults when you run it, and
    an implementation that doesn't actually support recursion. I wonder if it
    would be better to discuss these problems with the Whale authors, rather
    than write about them in your paper. It might just be a small bug in Whale
    that is easily fixed. If you can get Whale working, and do a proper
    comparison of it with your tool, then your Related Work discussion could
    become much more meaningful.

