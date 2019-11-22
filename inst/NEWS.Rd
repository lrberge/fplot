\name{NEWS}
\alias{NEWS}

\title{News for \R Package \pkg{fplot}}

\section{Changes in version 0.X.X}{
    \subsection{Minor user visible changes.}{
        \itemize{
            \item
        }
    }
}

\section{Changes in version 0.2.0 (2019-11-21)}{
    \subsection{Major improvements}{
        \itemize{
            \item plot_distr: A new method for displaying moderators: mod.method = "stack".
            \item plot_distr: A new argument "cumul" to represent cumulative distributions.
            \item \code{mod.method = "splitXX"} is now supported with logarithmic data
            \item \code{mod.method = "splitXX"} + \code{toLog=TRUE}: now supports the "other" column.
            \item \code{labels.tilted=TRUE} now possible with logarithmic data.
        }
    }

    \subsection{Other improvements to plot_distr}{
        \itemize{
            \item when onTop != "none", very small numbers are not displayed any more (could lead to cluttering).
            \item when onTop can also be equal to FALSE.
            \item Now discrete ticks appear when plotting categorical data. They appear every 5/10 bins, in order to help counting the bins.
            \item better labeling of the y-axis when \code{yaxis.num = TRUE}.
            \item better automatic finding of the number of bins when argument bin was not missing.
            \item better algorithm to find the default bin.size.
            \item when bin.size is given, toLog's default is automatically set off.
        }
    }

    \subsection{Improvements to plot_lines}{
        \itemize{
            \item Argument mod.select introduced to easily select moderator cases.
            \item When smoothing_window > 1, supplemental years are removed.
        }
    }

    \subsection{Data sets}{
        \itemize{
            \item Data on US publications in biology is removed.
            \item Data on US publications in economics is added (much lighter than biology publications).
        }
    }


    \subsection{Bug correction}{
        \itemize{
            \item \code{yaxis.num = TRUE} in the presence of moderators could cause problems in special cases.
            \item Display problem when \code{mod.method = "splitxx"} and factor variables.
            \item Display problem when \code{mod.method = "sidexx"} and with the other columns (problem in the bar cutting).
            \item plot_distr: with moderators and mod.method = "sideWithin": rare bug corrected (identical values could be considered different).
        }
    }

    \subsection{Relabeling of arguments}{
        \itemize{
            \item Argument \code{bin} becomes \code{bin.size}.
            \item in argument \code{mod.method}, the value \code{within} and \code{total} become \code{sideWithin} and \code{sideTotal}.
        }
    }

}

\section{Changes in version 0.1.0 (2019-11-01)}{
    \subsection{First version!}{
        \itemize{
            \item This package is an effort to provide a simple class of plotting functions which: i) perform common operations in exploratory analyses, ii) have a compact syntax thanks to formulas, allowing aggregate/conditional/weighted graphs with minimum effort, iii) provide automatic options setting to best fit the data, iv) have a nice looking layout, and v) always take advantage of the full plotting region (without changing the graphical parameters) to export beautiful graphs with no/minimum tweak.
        }
    }
}
