\usepackage{proof}
https://www.logicmatters.net/resources/ndexamples/proofsty.html
or
\usepackage{semantic}
http://tug.ctan.org/macros/latex/contrib/semantic/semantic.pdf

TyDe review of union types:
---------------------------
A:
1. Pages mostly filled with code.
2. Big picture of algorithm
3. Only Int or Only String case
4. Judgemental presentation of the algorithm as a figure
5. References to (un)related work:
    - Make a claim why XDuce/Comega is less general and inferior.
    - gradual typing
  Rebuttal points:
    - the paper presents novel research, while it seems judged on the basis of purely pedagogic value
    - author feels disappointed that reviewer did not notice the benefits of Typelike
      presentation vs classic judgemental presentation
    - reinforce benefits of algorithmic Typelike presentation
      instead of judgemental (relational) presentation with uncertain order of computation
    - beyond set is novel in this work (AFAIR)
    - reviewer is set in the past, and unready to review or understand novelty of the paper

B:
1. Confused about "principal type property", subsumption relation, and well-formed types.
2. Why overgeneralisation is suitable as long as `Typelike` properties are preserved.
3. "laws of soundness... are abandoned in practice of larger systems"
4. Explain "levitated bounded semilattice"
5. Slightly repetitive code

C:
1. Note derivative literature with respect of JSON Autotype (like https://openproceedings.org/2017/conf/edbt/paper-62.pdf), note classical references in DB/XML community to note their inferiority.
2. Generic "tell me the target" writing suggests that reviewer never reads introduction and abstract.
3. Emphasize "desired outcome" of type inference.
---------------------------
