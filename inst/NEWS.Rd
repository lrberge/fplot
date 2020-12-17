\name{NEWS}
\alias{NEWS}

\title{News for \R Package \pkg{fplot}}

\section{Changes in version 1.0.1 (2020-xx-xx)}{
    \subsection{Bug fixes}{
        \itemize{
            \item Bug in \code{plot_box} when a call of the form \code{plot_box(var ~ 1, base)} was used.
            \item Bug in \code{plot_box} when a call of the form \code{plot_box(. ~ 1, base)} was used.
        }
    }
}

\section{Changes in version 1.0.0 (2020-05-06)}{

    \subsection{New set of functions to export graphs}{
        \itemize{
            \item A new set of functions has been created to easily export figures that should fit in a document.
            \item The five functions are \code{pdf_fit}, \code{png_fit}, \code{jpeg_fit}, \code{tiff_fit} and \code{bmp_fit}.
            \item First, if needed, you set the geometry of your document with \code{setFplot_page}. Defaults to US-letter and *normal* margins => this is used to deduce the size the figures will take in the document.
            \item When using \code{pdf_fit}, instead of providing the height and width of the figure in absolute terms, you provide the fraction of the text-width the figure should take, and the target font-size at which the plotting text should be rendered. The size of the plotting text, once the figure is in the final document, is guaranteed.
            \item Use \code{fit.off} instead of \code{dev.off} to close the device and you will get the exported graph displayed in the viewer pane! No need to open your file to see whether the export is proper, you have it automatically done!
        }
    }


    \subsection{New function: setting default values for \code{plot_distr}}{
        \itemize{
            \item New function \code{setFplot_distr} to set most default values of the function \code{plot_distr}.
        }
    }


    \subsection{User visible changes in \code{plot_distr}}{
        \itemize{
            \item MAJOR CHANGE NOT RETRO COMPATIBLE: complete overhaul of the formula. Now the main argument 'fml' is of the form \code{weights ~ var | moderator} (before it was \code{var ~ moderator | weights}). Allows to use one-sided formulas.
            \item Argument \code{tick_5} becomes \code{at_5} and can take three values: FALSE, "roman" or "line". It is used to inform on the index of the bars by adding a small Roman number under every 5 bars (\code{at_5 = "roman"}), or drawing a thick axis line every 5 bars (\code{at_5 = "line"}). The default depends on the types of the data.
            \item When fractions are displayed on top of the bars, very low numbers are now trucated (avoids having to display too small numbers).
            \item The name of the moderator is now displayed in the yaxis when appropriate for within distributions.
            \item Legend titles are now always displayed by default.
            \item You can display the distribution for several weights at the same time.
            \item Argument \code{mod.method} has been revamped. It's meaning has been split: \code{mod.method} takes values in "side", "split" or "stack" and two new arguments \code{within} and \code{total} have been created to define whether the distribution should be within each moderator or over the total distribution.
            \item The argument \code{trunc.method} now takes the values "auto", "right" and "mid" (instead of "auto", "trimRight" and "trimMid").
            \item The size of the bottom and left margins are now automatically adjusted so the text always fit.
            \item Argument \code{mod.select} now supports regular expressions.
            \item New argument \code{mod.NA} which display the distribution of missing values.
        }
    }

    \subsection{User visible changes in \code{plot_lines}}{
        \itemize{
            \item You can display the trend for several variables at the same time => they will be treated as moderators.
            \item One sided formulas are accepted.
            \item Argument \code{mod.select} now supports regular expressions.
            \item New argument \code{mod.NA} which display the distribution of missing values.
        }
    }

    \subsection{User visible changes in \code{plot_box}}{
        \itemize{
            \item You can display several variables at once (long overdue!).
        }
    }

    \subsection{Error-handling}{
        \itemize{
            \item Better error-handling with dreamerr's functions.
        }
    }

    \subsection{Bug correction}{
        \itemize{
            \item[plot_lines] Bug when plotting frequencies with moderators.
            \item[plot_box] Bug regarding the display of the outliers when the cases are of type factor.
            \item[plot_distr] Bug when the variable was of type logical.
            \item[fplot] Bug when the moderator was loigical.
        }
    }


    \subsection{Arguments: name change}{
        \itemize{
            \item max_line => line.max
            \item tick_5 => at_5 (and is much better implemented)
            \item maxFirst => sorted
            \item onTop => top
            \item addOther => other
            \item maxBins => nbins
            \item toLog => log
        }
    }
}

\section{Changes in version 0.2.1 (2020-02-12)}{

    \subsection{User visible changes}{
        \itemize{
            \item Better adjustment of the legend height.
        }
    }

    \subsection{Bug fixes}{
        \itemize{
            \item[plot_distr] Bug when the weight contained NAs. Now fixed.
        }
    }
    \subsection{Other}{
        \itemize{
            \item Package \code{fixest} is removed from help files and called conditionnaly in the vignette.
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
