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
