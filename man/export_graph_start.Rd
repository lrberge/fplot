% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscfuns.R
\name{export_graph_start}
\alias{export_graph_start}
\alias{export_graph_end}
\title{Graph export with garanteed text size}
\usage{
export_graph_start(
  file = NULL,
  pt = 10,
  width = 1,
  height,
  w2h = 1.75,
  h2w,
  sideways = FALSE,
  res = 300,
  type = NULL,
  margin = NULL,
  margin.left = NULL,
  margin.right = NULL,
  margin.top = NULL,
  margin.bottom = NULL,
  margin.unit = "line",
  box = NULL,
  col.bg = NULL,
  col.default = NULL,
  yaxis.horiz = NULL,
  lwd = NULL,
  outermargin = NULL,
  outermargin.left = NULL,
  outermargin.right = NULL,
  outermargin.top = NULL,
  outermargin.bottom = NULL,
  outermargin.unit = "line",
  square_plot = NULL,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  title.size = NULL,
  title.col = NULL,
  title.bold = NULL,
  title.italic = NULL,
  axis.size = NULL,
  axis.col = NULL,
  axis.bold = NULL,
  axis.italic = NULL,
  label.size = NULL,
  label.col = NULL,
  label.bold = NULL,
  label.italic = NULL,
  ...
)

export_graph_end()
}
\arguments{
\item{file}{Character scalar or \code{NULL}. The name of the file in which to save the graph.
If the argument \code{type} is \code{NULL}, the type of file is deduced from the extension.
If your file extension is different from your file type, you need to use the
argument \code{type}. It \code{file = NULL}, no graph is exported but, if provided,
the graphical parameters are modified accordingly.}

\item{pt}{The size of the text, in pt, once the figure is inserted in your final document.
The default is 10. This means that all text appearing in the plot with \code{cex = 1}
will appear with 10pt-sized fonts in your document.}

\item{width}{The width of the graph, expressed in percentage of the width of
the body-text of the document in which it will be inserted. Default is 1, which means
that the graph will take 100\% of the text width. It can also be equal to a character
of the type \code{"100\%"} or \code{"80\%"}. Alternatively, the following units
are valid. Relative sizes: \code{"pw"} (page width), \code{"tw"} (text width),
\code{"ph"} (page height), \code{"th"} (text height).
Absolute sizes: \code{"in"}, \code{"cm"}, and \code{"px"}.}

\item{height}{Numeric between 0 and 1 or character scalar. The height of the graph,
expressed in percentage of the height of the body-text of the document in which it
will be inserted. Default is missing, and the height is determined by the other
argument \code{w2h}. This argument should range between 0 and 1. It can also be
equal to a character of the type \code{"100\%"} or \code{"80\%"}. Alternatively, the
following units are valid. Relative sizes: \code{"pw"} (page width), \code{"tw"}
(text width), \code{"ph"} (page height), \code{"th"} (text height). Absolute
sizes: \code{"in"}, \code{"cm"}, and \code{"px"}.}

\item{w2h}{Numeric scalar. Used to determine the height of the figure based on
the width. By default it is equal to \code{1.75} which means that the graph
will be 1.75 larger than tall. Note that when argument \code{sideways = TRUE},
the default for the height becomes \verb{90\%}.}

\item{h2w}{Numeric scalar, default is missing. Used to determine the aspectr ratio of the figure.}

\item{sideways}{Logical, defaults to \code{FALSE}. If the figure will be placed in
landscape in the final document, then \code{sideways} should be equal to \code{TRUE}.
If TRUE, then the argument \code{width} now refers to the height of the text, and the
argument \code{height} to its width.}

\item{res}{Numeric, the resolution in ppi. Default is 300.}

\item{type}{Character scalar, default is \code{NULL}. The type of file to be created.
If \code{NULL}, the default, then the type of file is deduced from the extension.}

\item{margin}{Numeric vector, default is \code{NULL}. Defines the size of the four
plotting margins (in this order: bottom, left, top, right).
If of length 1 or 2, the content is recycled to fit 4 elements.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{margin.left}{Numeric scalar, default is \code{NULL}. The size of the left margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{margin.right}{Numeric scalar, default is \code{NULL}. The size of the right margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{margin.top}{Numeric scalar, default is \code{NULL}. The size of the top margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{margin.bottom}{Numeric scalar, default is \code{NULL}. The size of the bottom margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{margin.unit}{Character scalar equal to either: i) "line" (default), ii) "inch", iii) "cm".}

\item{box}{Can be equal to \code{NULL} (default), a logical scalar, or a character scalar.
Defines how to draw the box around the plotting region.
If a logical, \code{TRUE} means that all borders are drawn, and \code{FALSE} means none.
If a character scalar, it should contain the following letters: "b", "l", "t" and/or "r",
which stand for the bottom, left, top and right border.}

\item{col.bg}{An R color, default is \code{NULL}. The background color of the plot.}

\item{col.default}{An R colors, default is \code{NULL}. They represent the default color
used for plotting (the axes will be drawn with that color and it will be the default
color when the argument \code{col} is not provided in plotting functions).}

\item{yaxis.horiz}{Logical, default is \code{NULL}. Whether to display the y-axis labels
horizontally.}

\item{lwd}{Numeric scalar, default is \code{NULL}. The default width of the lines.}

\item{outermargin}{Numeric vector, default is \code{NULL}. Defines the size of the four
outer margins. If of length 1 or 2, the content is recycled to fit 4 elements.
By default the unit is the "line" but you can change it with the argument
\code{outermargin.unit}.}

\item{outermargin.left}{Numeric scalar, default is \code{NULL}. The size of the left outer margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{outermargin.right}{Numeric scalar, default is \code{NULL}. The size of the right outer margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{outermargin.top}{Numeric scalar, default is \code{NULL}. The size of the top outer margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{outermargin.bottom}{Numeric scalar, default is \code{NULL}. The size of the bottom outer margin.
By default the unit is the "line" but you can change it with the argument
\code{margin.unit}.}

\item{outermargin.unit}{Character scalar equal to either: i) "line" (default),
ii) "inch", iii) "cm".}

\item{square_plot}{Logical, default is \code{NULL}. Whether the plotting region should fit
a square. If \code{FALSE} the plotting region is maximal.}

\item{nrow}{Integer scalar, default is \code{NULL}. To display multiple graphs, the number of
rows of the graphical matrix.}

\item{ncol}{Integer scalar, default is \code{NULL}. To display multiple graphs, the number of
columns of the graphical matrix.}

\item{byrow}{Logical, default is TRUE. When plotting multiple graphs on a graphical matrix,
whether to plot the graphs by row.}

\item{title.size}{Numeric scalar, default is \code{NULL}. Values greater than 1 increase
the size of the plot titles, values lower than 1 reduce them.}

\item{title.col}{An R color, default is \code{NULL}. The color for the graph titles.}

\item{title.bold}{Logical, default is \code{NULL}. Whether to display the title in bold font.}

\item{title.italic}{Logical, default is \code{NULL}. Whether to display the title in italic font.}

\item{axis.size}{Numeric scalar, default is \code{NULL}. Values greater than 1 increase
the size of the text in the plot axes, values lower than 1 reduce them.}

\item{axis.col}{An R color, default is \code{NULL}. The color for the text in the graph axes.}

\item{axis.bold}{Logical, default is \code{NULL}. Whether to display the axis's text in bold font.}

\item{axis.italic}{Logical, default is \code{NULL}. Whether to display the axis's text in italic font.}

\item{label.size}{Numeric scalar, default is \code{NULL}. Values greater than 1 increase
the size of the plot labels, values lower than 1 reduce them.}

\item{label.col}{An R color, default is \code{NULL}. The color for the graph labels.}

\item{label.bold}{Logical, default is \code{NULL}. Whether to display the labels in bold font.}

\item{label.italic}{Logical, default is \code{NULL}. Whether to display the labels in italic font.}

\item{...}{Other arguments to be passed to \code{\link[grDevices:png]{bmp}},
\code{\link[grDevices]{png}}, \code{\link[grDevices:png]{jpeg}}, or
\code{\link[grDevices:png]{tiff}}. For example: \code{antialias}, \code{bg}, etc.}
}
\value{
These functions do not return anything in R. \code{export_graph_start} creates a
file linked to the R graphics engine, in which subsequent plots are saved.
\code{export_graph_end} closes the connection and the file.
}
\description{
This function facilitates graph exportation by taking into account the final
destination of the graph (typically a document) and allowing the user to use
point size, an intuitive unit
in written documents, as the graph scaler. Once located in the final document, the text of the graph
at the default size will be at the defined point size.
}
\details{
When the function \code{export_graph_end()} is called, the resulting exported graph
is displayed in the Viewer. The viewer function is found with
\code{getOption("viewer")} and should work on RStudio and VSCode (with the R extension).

When the graphical parameters are modified in \code{export_graph_start}, they are reset
once a call to \code{export_graph_end}, or new call to \code{export_graph_start}, is run.

When the argument \code{file = NULL} (default), the function \code{export_graph_start} can be
used in lieu of \code{par}, possibly facilitating the reset of the graphical parameters.

To export a ggplot2 graph, remember that you need to \strong{print} it!

\if{html}{\out{<div class="sourceCode">}}\preformatted{library(ggplot2)
data = data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))

# NOT GOOD
export_graph_start("test.pdf")
ggplot(data, aes(x, y)) +
  geom_point(color = "#54BF98") +
  geom_line(color = "#d34661")
export_graph_end()

# GOOD
my_graph = ggplot(data, aes(x, y)) +
             geom_point(color = "#54BF98") +
             geom_line(color = "#d34661")

export_graph_start("test.pdf")
print(my_graph)
export_graph_end()
}\if{html}{\out{</div>}}
}
\section{Functions}{
\itemize{
\item \code{export_graph_end()}: Ends the connection to the current export and creates the file.

}}
\section{Setting the page size}{


You can set the page size with the function \code{\link[fplot]{setFplot_page}},
which defines the size of the page and its margins to deduce the size of the body
of the text in which the figures will be inserted. By default the page is considered
to be US-letter with \emph{normal} margins (not too big nor thin).

It is important to set the page size appropriately to have a final plotting-text size
guaranteed once the figure is inserted in the document.
}

\examples{

tmpFile = file.path(tempdir(), "png_examples.pdf")

# we start the exportation
export_graph_start(tmpFile, pt = 8)

plot(1, 1, type = "n", ann = FALSE)
text(1, 1, "This text will be displayed in 8pt.")

# the line below closes the connection and displays the 
# graph in the viewer pane if appropritate
export_graph_end()

# We create a 'normal' graph where we change a few graphical parameters
export_graph_start(col.default = "deepskyblue1", title.col = "indianred1", 
                   title.italic = TRUE, title.size = 2)
with(iris, plot(Sepal.Length, Petal.Length, pch = 18, cex = 2))
title("Iris data set")
# we reset the graphical parameters:
export_graph_end()


}
\seealso{
The tool to set the page size and the exporting defaults: \code{\link[fplot]{setFplot_page}}.
Exporting functions \code{\link[fplot]{pdf_fit}}, \code{\link[fplot:pdf_fit]{png_fit}},
\code{\link[fplot:pdf_fit]{jpeg_fit}}.

The functions \code{\link[=export_graph_start]{export_graph_start()}} and \code{\link[=export_graph_end]{export_graph_end()}} provide similar features.
}
\author{
Laurent Berge
}
