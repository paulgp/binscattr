# binscattr

* Todo:
  1) Rotate positions around graph
  2) Move regressions inside discrete vs not, so it's not wastefully running regs
	  * If the regression is discrete, we don't need to residualize for the controls, and x and y don't get replaced. Might not be a big deal
  3) <s>Figure out why the text looks so crappy</s>
  4) <s>Connected line graphs</s> 
  5) label graphs
  6) <s>Change input syntax using dplyr quoting conventions (http://dplyr.tidyverse.org/articles/programming.html) </s>
  7) <s>Allow for factored control variables</s>
  8) <s>Bin scatter by group</s>
  9) Allow for splitting up binning/line fit at user-provided x-breaks (a la Michael Stepner's RD example)
  10) Make connectlines default to false when fitline is true and vice versa (unless both are specified by the user)
  11) Handle data with missing values(?)


* Unit Test Todos

** Check inputs

	1) do variable types line-up correctly?

** identify by groups

** construct regression formula

*** Test cases: 

	1) no controls
	2) controls + no absorb
	3) no contorls + absorb
	4) controls + absorb
	5) multiple absorbs
	6) missing values
	7) strings vs. numerics
	8) factor variables

** residualize variables

*** Test cases: 

	1) want to test that mean(X_res) \approx 0, X_res, Y_res are uncorrelated with controls
	2) Repeat test cases from regression formulaes
	3) Compare regression coefficients from full multivariate regression to residual regression coefficients

** plotting scatter points

** plotting regression line 

** connect dots

** write coefficient and standard error on graph

