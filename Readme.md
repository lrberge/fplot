<a href="https://cran.r-project.org/web/checks/check_results_fplot.html"><img src="https://cranchecks.info/badges/flavor/release/fplot" alt="CRAN status"></a>
<a href="https://cran.r-project.org/package=fplot"><img src="http://www.r-pkg.org/badges/version/fplot" alt="Version"> </a>
<a href="https://ipub.com/dev-corner/apps/r-package-downloads/"> <img src="https://cranlogs.r-pkg.org/badges/fplot" alt = "Downloads"> </a>

`fplot` is an `R` package made for the easy plotting of distributions. 

The problem when it comes to distributions is that we often face a variety of data, each requiring a different type of layout: we don't represent the distribution of categorical variables similarly to continuous variables, and regarding the latter heavily skewed data may require a special treatment. The aim of `fplot` is to automatically make choices for the user in order to always display meaningful (and hopefully beautiful!) graphs. 

This wiki page shows a brief gallery of `fplot` possibilities. For a more detailed introduction, please see the [walk-through](https://cran.r-project.org/package=fplot/vignettes/fplot_walkthrough.html).

The data sets used are `us_pub_econ` (from `fplot`) relating to publications in economics from US institutions, `trade` (from package `fixest`) relating to trade value for some products in the European Union, and the `iris` data set (from base `R`).

One aim of `fplot`is to easily export graphs, it therefore includes a tool to relabel the variable names *globally* (once and for all!). The following line is run before plotting the graphs:

`setFplot_dict(c(Origin = "Exporting Country", Destination = "Importing Country", Euros = "Exports Value in €", jnl_top_5p = "Publications in Top 5% journal", journal = "Journal", institution = "U.S. Institution", Petal.Length = "Petal Length"))` 

The code to run the plots are in the titles of the graphs. You'll notice that the same command line can result in a various set of graphs. 

![Figure 1: Distribution of US institutions production (by number of publications in economics).](https://github.com/lrberge/fplot/blob/master/vignettes/images/regular_pub.png)

![Figure 2: Distribution of exportation volumes.](https://github.com/lrberge/fplot/blob/master/vignettes/images/regular_trade.png)

![Figure 3: Distribution of petal length.](https://github.com/lrberge/fplot/blob/master/vignettes/images/regular_iris.png)

![Figure 4: Conditional distribution of US institutions by journal.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_pub_1.png)

![Figure 5: Conditional distribution of US institutions by journal, for Cell, Nature and Science.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_pub_2.png)

![Figure 6: Conditional distribution of US institutions by journal, for Cell, Nature and Science -- different layout.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_pub_3_bis.png)

![Figure 7: Conditional distribution of exportation value by exporting country.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_trade_1.png)

![Figure 8: Conditional distribution of exportation value by exporting country -- different layout.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_trade_2.png)

![Figure 9: Conditional distribution of petal length by species.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_iris_1.png)

![Figure 10: Conditional distribution of petal length by species -- different layout.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_iris_2.png)

![Figure 10: Conditional distribution of petal length by species -- Stacked.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_iris_3.png)

![Figure 11: Distribution of US institutions production, weighted by journal quality.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_pub_1.png)

![Figure 12: Distribution of US institutions for different measures of production.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_pub_2.png)

![Figure 13: Distribution of exporting value by exporting country.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_trade_1.png)

![Figure 14: Distribution of exporting value by exporting country -- export value on top of the bars.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_trade_2.png)

![Figure 15: Distribution of exporting value by importing country, the sample is split by the top exporters.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_trade_1.png)

![Figure 16: Distribution of exporting value by importing country, the sample is split by the top exporters -- bar for the "other" group.](https://github.com/lrberge/fplot/blob/master/vignettes/images/conditional_trade_2.png)

![Figure 17: Conditional production of US institution, conditional on year, stacked.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_stacked_pub.png)

![Figure 18: Distribution of exporting value by importing country, the sample is split by the top exporters -- Stacked.](https://github.com/lrberge/fplot/blob/master/vignettes/images/weighted_stacked_trade.png)

![Figure 19: Cumulative distribution of publications in top 5% journals.](https://github.com/lrberge/fplot/blob/master/vignettes/images/cumul_pub.png)

![Figure 19: Cumulative distribution of exportations by value.](https://github.com/lrberge/fplot/blob/master/vignettes/images/cumul_trade.png)


