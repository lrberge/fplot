% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscfuns.R
\docType{data}
\name{setFplot_dict}
\alias{setFplot_dict}
\alias{getFplot_dict}
\title{Sets/gets the dictionary used in \code{fplot}}
\format{
An object of class \code{function} of length 1.
}
\usage{
setFplot_dict(dict = NULL, ..., reset = FALSE)

getFplot_dict
}
\arguments{
\item{dict}{A named character vector or a character scalar. E.g. to change my variable named "a"
and "b" to (resp.) "$log(a)$" and "$bonus^3$", then use
\code{dict = c(a="$log(a)$", b3="$bonus^3$")}.}

\item{...}{You can add arguments of the form: \code{variable_name = "Definition"}. This is an
alternative to using a named vector in the argument \code{dict}.}

\item{reset}{Logical, default is \code{FALSE}. If \code{TRUE}, then the dictionary is reset. Note that the
default dictionary always relabels the variable "(Intercept)" in to "Constant". To overwrite it,
you need to add "(Intercept)" explicitly in your dictionary.}
}
\value{
The function \code{setFplot_dict()} does not return anything, it only sets an option after checking
the format of the arguments.

The function \code{getFplot_dict()} returns a named vector representing the
dictionary set in \code{setFplot_dict()}.
}
\description{
Sets/gets the default dictionary used to rename the axes/moderator variables
in the functions of the package \code{fplot}. The dictionaries are used to relabel
variables (usually towards a fancier, more explicit formatting) that can be useful
not to explicitly use the arguments xlab/ylab when exporting graphs. By setting
the dictionary with \code{setFplot_dict}, you can avoid providing the argument
\code{dict} in \code{fplot} functions.
}
\details{
By default the dictionary only grows. This means that successive calls with not erase the
previous definitions unless the argument \code{reset} has been set to \code{TRUE}.

The default dictionary is equivalent to having \code{setFplot_dict("(Intercept)" = "Constant")}. To
change this default, you need to provide a new definition to \code{"(Intercept)"} explicitly.

This dictionary is shared with the \code{fixest} package.
}
\examples{

data(airquality)
setFplot_dict(c(Ozone = "Ozone (ppb)"))
plot_distr(Ozone ~ Month, airquality, weight.fun = mean)

}
\author{
Laurent Berge
}
\keyword{datasets}
