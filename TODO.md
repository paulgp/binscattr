

* master file : binscattr.R

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

