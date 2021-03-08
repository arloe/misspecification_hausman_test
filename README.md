### Misspecification of hausman test

### Durbin-Wu-Hausman Test
Hausman test is a statistical test in econometrics.
Consider the linear model $$\mathbf{ y_{it}=\alpha+\beta^{T}X_{it}+u_{it} }$$ and $$\mathbf{ u_{it}=\mu_{i}+v_{it} }$$. $$\mathbf{v}$$ is error term and $$\mathbf{\mu}$$ is random effect.In this situation, two assumptions are possible. One is that $$\mathbf{\mu_{i}}$$ is uncorrelated with $$\mathbf{X_{it}}$$ and another assumption is that $$\mathbf{\mu_{i}}$$ is allowed to correlated with $$\mathbf{X_{it}}$$. Hausman test tests whether $$\mathbf{\mu_{i}}$$ and $$\mathbf{X_{it}}$$ are correlated or not.

### Hypothesis
$$\mathbf{H_{0}}$$: Covariates and random effects are uncorrelated, $$\mathbf{cov(\mu_{i}, x_{it}) = 0}$$
$$\mathbf{H_{1}}$$: Covariates and random effects are correlated, $$\mathbf{cov(\mu_{i}, x_{it}) \neq 0}$$

### Hausman statistic
<!-- H = (b_{1}-b_{0})^{'} (Var(b_{0})-var(b_{1}))^{+} (b_{1}-b_{0}) --> 
where $$^{+}$$ denotes the [Moore-Penrose pseudoinverse](https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse).

### Problem
In panel data analysis, hausman test used to test whether random effects model or fixed effects model.

|                      | $$\mathbf{cov(\mu_{i}, x_{it}) = 0}$$ | $$\mathbf{cov(\mu_{i}, x_{it}) \neq 0}$$ |
| :---:                |     :---:              |        :---: |
| Random Effects model | Consistent & Efficient | Inconsistent |
| Fixed Effects model  | Consistent             | Consistent   |

But, The hypothesis hausman test does not determine whether it is a random effect or a fixed effect. If the distribution of random effects is not normal, the performance of the Hausmann test was not confirmed. So, the experiment was conducted under several distribution assumptions.

### Usage
```R
# generate simulation data
df  = make_simulation_dataframe(N = 600, n = 5, family = 7)
# estimate model (Marginal MLE & Conditional MLE)
est = model_fit(formula_marginal = y ~ time*X + (1|id), formula_condition = y ~ time*X + strata(id), data = df)

# delete fixed covariate
coef_marginal = est$coef_marginal[-2]
coef_condition = est$coef_condition[-2]
var_marginal = est$var_marginal[-2, -2]
var_condition = est$var_condition[-2, -2]

# calculate p-value of hausman test
p_value = hausman_test(  coef_marginal = coef_marginal, coef_condition = coef_condition
                       , var_marginal = var_marginal, var_condition = var_condition)
```

### Reference
 * Tchetgen, E. J. and Coull, B. A. (2006). A diagnostic test for the mixing distribution in a generalised linear mixed model. Biometrika, 93:1003–1010.
 * Hausman, J. (1978). Specification tests in econometrics. Econometrica, 46:1251–1271.
 * Greene, William (2012). Econometric Analysis.