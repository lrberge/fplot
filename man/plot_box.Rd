% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{plot_box}
\alias{plot_box}
\title{Boxplots with possibly moderators}
\usage{
plot_box(
  fml,
  data,
  case,
  moderator,
  inCol,
  outCol = "black",
  density = -1,
  lty = 1,
  pch = 18,
  addLegend = TRUE,
  legend_options = list(),
  lwd = 2,
  outlier,
  dict = NULL,
  dict_case,
  dict_moderator,
  order_case,
  order_moderator,
  addMean,
  mean.col = "darkred",
  mean.pch = 18,
  mean.cex = 2,
  mod.title = TRUE,
  labels.tilted,
  trunc = 20,
  trunc.method = "auto",
  line.max,
  ...
)
}
\arguments{
\item{fml}{A numeric vector or a formula of the type:
\code{vars ~ moderator_1 | moderator_2}. Note that if a formula is provided then
the argument \sQuote{data} must be provided. You can plot several variables,
if you don't want a moderator, use 1 instead: e.g.
\code{plot_box(Petal.Width +Petal.Length ~ 1, iris)}. You can plot all numeric
variables from a data set using \code{"."}: \code{plot_box(. ~ 1, iris)}.}

\item{data}{A data.frame/data.table containing the relevant information.}

\item{case}{When argument fml is a vector, this argument can receive a vector of cases.}

\item{moderator}{When argument fml is a vector, this argument can receive a vector of moderators.}

\item{inCol}{A vector of colors that will be used for within the boxes.}

\item{outCol}{The color of the outer box. Default is black.}

\item{density}{The density of lines within the boxes. By default it is equal to -1,
which means the boxes are filled with color.}

\item{lty}{The type of lines for the border of the boxes. Default is 1 (solid line).}

\item{pch}{The patch of the outliers. Default is 18.}

\item{addLegend}{Default is \code{TRUE}. Should a legend be added at the top
of the graph is there is more than one moderator?}

\item{legend_options}{A list. Other options to be passed to \code{legend} which
concerns the legend for the moderator.}

\item{lwd}{The width of the lines making the boxes. Default is 2.}

\item{outlier}{Default is \code{TRUE}. Should the outliers be displayed?}

\item{dict}{A dictionnary to rename the variables names in the axes and legend.
Should be a named vector. By default it s the value of \code{getFplot_dict()},
which you can set with the function \code{\link[fplot]{setFplot_dict}}.}

\item{dict_case}{A named character vector. If provided, it changes the values
of the variable \sQuote{case} to the ones contained in the vector \code{dict_case}.
Example: to change the variable named "a" to "Australia" and "b" to "Brazil",
use \code{dict=c(a="Australia",b="Brazil")}.}

\item{dict_moderator}{A named character vector. If provided, it changes
the values of the variable \sQuote{moderator} to the ones contained in
the vector \code{dict_moderator}. Example: to change the variable
named "a" to "Australia" and "b" to "Brazil", use \code{dict=c(a="Australia",b="Brazil")}.}

\item{order_case}{Character vector. This element is used if the user wants the
\sQuote{case} values to be ordered in a certain way.
This should be a regular expression (see \code{\link[base]{regex}} help for more info).
There can be more than one regular expression. The variables satisfying
the first regular expression will be placed first, then the order follows
the sequence of regular expressions.}

\item{order_moderator}{Character vector. This element is used if the user wants
the \sQuote{moderator} values to be ordered in a certain way. This should be
a regular expression (see \code{\link[base]{regex}} help for more info).
There can be more than one regular expression. The variables satisfying the
first regular expression will be placed first, then the order follows the
sequence of regular expressions.}

\item{addMean}{Whether to add the average for each boxplot. Default is true.}

\item{mean.col}{The color of the mean. Default is darkred.}

\item{mean.pch}{The patch of the mean, default is 18.}

\item{mean.cex}{The cex of the mean, default is 2.}

\item{mod.title}{Character scalar. The title of the legend in case there is a
moderator. You can set it to \code{TRUE} (the default) to display the moderator
name. To display no title, set it to \code{NULL} or \code{FALSE}.}

\item{labels.tilted}{Whether there should be tilted labels. Default is \code{FALSE}
except when the data is split by moderators (see \code{mod.method}).}

\item{trunc}{If the main variable is a character, its values are truncaded to
\code{trunc} characters. Default is 20. You can set the truncation method with
the argument \code{trunc.method}.}

\item{trunc.method}{If the elements of the x-axis need to be truncated, this
is the truncation method. It can be "auto", "right" or "mid".}

\item{line.max}{Option for the x-axis, how far should the labels go.
Default is 1 for normal labels, 2 for tilted labels.}

\item{...}{Other parameters to be passed to \code{plot}.}
}
\value{
Invisibly returns the coordinates of the x-axis.
}
\description{
This function allows to draw a boxplot, with possibly separating different moderators.
}
\examples{

# Simple iris boxplot
plot(1:10)

# All numeric variables
plot_box(. ~ 1, iris)

# All numeric variable / splitting by species
plot_box(. ~ Species, iris)

# idem but with renaming
plot_box(. ~ Species, iris, dict = c(Species="Iris species",
         setosa="SETOSA", Petal.Width="Width (Petal)"))

# Now using two moderators
base = iris
base$period = sample(1:4, 150, TRUE)

plot_box(Petal.Length ~ period | Species, base)





}
\author{
Laurent Berge
}
