
# fplot 1.0.1

## New functions

- new pair of functions `export_graph_start` and `export_graph_end` to export graphs. This is similar to the family of functions `pdf_fit`, `png_fit`, etc, and `fit.off()`, but possibly more intuitive.

## Bug fixes

- Bug in `plot_box` when a call of the form `plot_box(var ~ 1, base)` was used.
- Bug in `plot_box` when a call of the form `plot_box(. ~ 1, base)` was used.

## Other

- update the maintainer's email address
- remove warning from `pdftools::pdf_convert` popping when exporting graphs.
- when exporting graphs, there is a preview of the exported graph. Now the image is embedded in the HTML file via URI to avoid file path issues (this led to a bug in the VSCode viewer).
- the documentation is updated to add clear guidance for `ggplot2` graph exportation
- improved documentation

# fplot 1.0.0 (2020-05-08)

## New set of functions to export graphs

- A new set of functions has been created to easily export figures that should fit in a document.

- The five functions are `pdf_fit`, `png_fit`, `jpeg_fit`, `tiff_fit` and `bmp_fit`.

- First, if needed, you set the geometry of your document with `setFplot_page`. Defaults to US-letter and *normal* margins => this is used to deduce the size the figures will take in the document.

- When using `pdf_fit`, instead of providing the height and width of the figure in absolute terms, you provide the fraction of the text-width the figure should take, and the target font-size at which the plotting text should be rendered. The size of the plotting text, once the figure is in the final document, is guaranteed.

- Use `fit.off` instead of `dev.off` to close the device and you will get the exported graph displayed in the viewer pane! No need to open your file to see whether the export is proper, you have it automatically done!

## New function: setting default values for `plot_distr`

- New function `setFplot_distr` to set most default values of the function `plot_distr`.

## User visible changes in `plot_distr`

- MAJOR CHANGE NOT RETRO COMPATIBLE: complete overhaul of the formula. Now the main argument 'fml' is of the form `weights ~ var | moderator` (before it was `var ~ moderator | weights`). Allows to use one-sided formulas.
- Argument `tick_5` becomes `at_5` and can take three values: FALSE, "roman" or "line". It is used to inform on the index of the bars by adding a small Roman number under every 5 bars (`at_5 = "roman"`), or drawing a thick axis line every 5 bars (`at_5 = "line"`). The default depends on the types of the data.
- When fractions are displayed on top of the bars, very low numbers are now trucated (avoids having to display too small numbers).

- The name of the moderator is now displayed in the yaxis when appropriate for within distributions.
- Legend titles are now always displayed by default.
- You can display the distribution for several weights at the same time.
- Argument `mod.method` has been revamped. It's meaning has been split: `mod.method` takes values in "side", "split" or "stack" and two new arguments `within` and `total` have been created to define whether the distribution should be within each moderator or over the total distribution.
- The argument `trunc.method` now takes the values "auto", "right" and "mid" (instead of "auto", "trimRight" and "trimMid").
- The size of the bottom and left margins are now automatically adjusted so the text always fit.
- Argument `mod.select` now supports regular expressions.
- New argument `mod.NA` which display the distribution of missing values.

## User visible changes in `plot_lines`

- You can display the trend for several variables at the same time => they will be treated as moderators.
- One sided formulas are accepted.
- Argument `mod.select` now supports regular expressions.
- New argument `mod.NA` which display the distribution of missing values.

## User visible changes in `plot_box`

- You can display several variables at once (long overdue!).

## Error-handling

- Better error-handling with dreamerr's functions.

## Bug correction

- plot_lines: Bug when plotting frequencies with moderators.
- plot_box: Bug regarding the display of the outliers when the cases are of type factor.
- plot_distr: Bug when the variable was of type logical.
- fplot: Bug when the moderator was loigical.


## Arguments: name change

- max_line => line.max
- tick_5 => at_5 (and is much better implemented)
- maxFirst => sorted
- onTop => top
- addOther => other
- maxBins => nbins
- toLog => log

# Changes in version 0.2.1 (2020-02-12)

## User visible changes

- Better adjustment of the legend height.

## Bug fixes

- plot_distr: Bug when the weight contained NAs. Now fixed.

## Other

- Package `fixest` is removed from help files and called conditionnaly in the vignette.

# Changes in version 0.2.0 (2019-11-21)

## Major improvements

- plot_distr: A new method for displaying moderators: mod.method = "stack".
- plot_distr: A new argument "cumul" to represent cumulative distributions.
- `mod.method = "splitXX"` is now supported with logarithmic data
- `mod.method = "splitXX"` + `toLog=TRUE`: now supports the "other" column.
- `labels.tilted=TRUE` now possible with logarithmic data.

## Other improvements to plot_distr

- when onTop != "none", very small numbers are not displayed any more (could lead to cluttering).
- when onTop can also be equal to FALSE.
- Now discrete ticks appear when plotting categorical data. They appear every 5/10 bins, in order to help counting the bins.
- better labeling of the y-axis when `yaxis.num = TRUE`.
- better automatic finding of the number of bins when argument bin was not missing.
- better algorithm to find the default bin.size.
- when bin.size is given, toLog's default is automatically set off.

## Improvements to plot_lines

- Argument mod.select introduced to easily select moderator cases.
- When smoothing_window > 1, supplemental years are removed.

## Data sets

- Data on US publications in biology is removed.
- Data on US publications in economics is added (much lighter than biology publications).


## Bug correction

- `yaxis.num = TRUE` in the presence of moderators could cause problems in special cases.
- Display problem when `mod.method = "splitxx"` and factor variables.
- Display problem when `mod.method = "sidexx"` and with the other columns (problem in the bar cutting).
- plot_distr: with moderators and mod.method = "sideWithin": rare bug corrected (identical values could be considered different).

## Relabeling of arguments

- Argument `bin` becomes `bin.size`.
- in argument `mod.method`, the value `within` and `total` become `sideWithin` and `sideTotal`.


# Changes in version 0.1.0 (2019-11-01)

## First version!

- This package is an effort to provide a simple class of plotting functions which: i) perform common operations in exploratory analyses, ii) have a compact syntax thanks to formulas, allowing aggregate/conditional/weighted graphs with minimum effort, iii) provide automatic options setting to best fit the data, iv) have a nice looking layout, and v) always take advantage of the full plotting region (without changing the graphical parameters) to export beautiful graphs with no/minimum tweak.
