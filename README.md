# Two group t-test operator

##### Description

`twogroupttest` operator performs a Student's t-test on the data.

##### Usage

Input projection|.
---|---
`y-axis`  | measurement value
`x-axis`  | represents the two groups to compare
`color`   | represents the pairing

Input parameters|.
---|---
`reverse`   | logical, indicating whether to reverse the calculation on the x-axis, default `FALSE`
`paired`    | logical, indicating whether to perform pairing, default `FALSE`
`alternative`   | A character string specifying the alternative hypothesis, default is "two.sided"
`mu`  | A number indicating the true value of the mean (or difference in means if you are performing a two sample test), default 0.0
`var.equal`  |logical, indicating whether to treat the two variances as being equal. If `TRUE` then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used, default `FALSE`
`conf.level`  |numeric, confidence level of the interval, default 0.95

Output relations|.
---|---
`p_value`| numeric, p-value calculated per cell

##### Details

The operator is the `t.test` function in base R.

##### References

See the `base::t.test` function of the R package for the documentation, 

##### See Also

[anova](https://github.com/tercen/anova_operator), [rfImp](https://github.com/tercen/rfImp_operator)


