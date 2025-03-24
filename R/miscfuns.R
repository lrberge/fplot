#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Sep 30 10:12:36 2019
# ~: misc. internal funs
#----------------------------------------------#


####
#### User visible funs. ####
####


#' Sets/gets the dictionary used in \code{fplot}
#'
#' Sets/gets the default dictionary used to rename the axes/moderator variables 
#' in the functions of the package \code{fplot}. The dictionaries are used to relabel 
#' variables (usually towards a fancier, more explicit formatting) that can be useful 
#' not to explicitly use the arguments xlab/ylab when exporting graphs. By setting 
#' the dictionary with \code{setFplot_dict}, you can avoid providing the argument 
#' \code{dict} in \code{fplot} functions.
#'
#'
#' @param dict A named character vector. E.g. to change my variable named "us_md" 
#' and "state" to (resp.) "$ miilion" and "U.S. state", then use 
#' \code{dict = c(us_md="$ million", state = "U.S. state")}.
#'
#' @author
#' Laurent Berge
#' 
#' @details 
#' This function stores a named vector in the option "fplot_dict".
#' The dictionary is automatically accessed by all `fplot` functions.
#' 
#' @return 
#' The function `setFplot_dict()` does not return anything, it only sets an option after checking 
#' the format of the arguments.
#' 
#' The function `getFplot_dict()` returns a named vector representing the 
#' dictionary set in `setFplot_dict()`.
#'
#' @examples
#'
#' data(airquality)
#' setFplot_dict(c(Ozone = "Ozone (ppb)"))
#' plot_distr(Ozone ~ Month, airquality, weight.fun = mean)
#'
setFplot_dict = function(dict){

    if(missing(dict) || is.null(dict)){
        options("fplot_dict" = NULL)
        return(invisible())
    }

    #
    # Controls
    #

    check_arg(dict, "named character vector")

    # Formatting the names
    dict_names = names(dict)
    dict_names = gsub(" +", "", dict_names)
    td = table(dict_names)
    if(any(td > 1)){
        qui = which(dict_names %in% names(td)[td > 1])
        name_dup = unique(names(dict)[qui])
        stop("Argument 'dict' contains duplicated names: ", enumerate_items(name_dup))
    }

    options("fplot_dict" = dict)
}

#' @rdname setFplot_dict
"getFplot_dict"

getFplot_dict = function(){

    x = getOption("fplot_dict")
    if(length(x) > 0){
        if(!is.character(x) || !checkVector(x) || anyNA(x)){
            stop("The value of getOption(\"fplot_dict\") is currently not legal. Please use function setFplot_dict to set it to an appropriate value. ")
        }
    }

    x
}


#' Sets the target page size for figure exporting
#'
#' Tha package \code{fplot} offers some functions (e.g. \code{\link[fplot]{pdf_fit}} 
#' or \code{\link[fplot]{png_fit}}) to export figures, with a guarantee to obtain 
#' the desired point size for the plotting text. The function \code{setFplot_page} 
#' sets the target page size (once and for all). This is important for the accuracy 
#' of the export, although the default values should be working well most of the time.
#'
#' @inheritParams pdf_fit
#'
#' @param page What is the page size of the document? Can be equal to "us" (for 
#' US letter, the default) or "a4". Can also be a numeric vector of length 2 giving 
#' the width and the height of the page in **inches**. Or can be a character string 
#' of the type: \code{"8.5in,11in"} where the width and height are separated with 
#' a comma, note that only centimeters (cm), inches (in) and pixels (px) are accepted 
#' as units--further: you can use the unit only once.
#' @param margins The bottom/left/top/right margins of the page. This is used to 
#' obtain the dimension of the body of the text. Can be equal to "normal" (default, 
#' which corresponds to 2cm/2.5cm/2cm/2.5cm), or to "thin" (1.5/1/1/1cm). Can be 
#' a numeric vector of length 1: then all margins are the same given size in **inches**. 
#' 
#' Can also be a numeric vector of length 2 or 4: 2 means first bottom/top margins, 
#' then left/right margins; 4 is bottom/left/top/right margins, in inches. Last, 
#' it can be a character vector of the type \code{"2,2.5,2,2.5cm"} with the margins 
#' separated by a comma or a slash, and at least one unit appearing: either \code{cm}, 
#' \code{in} or \code{px}.
#' @param units The default units when using the functions \code{\link[fplot]{pdf_fit}}, 
#' \code{\link[fplot]{png_fit}}, etc. Defaults to \code{"tw"} (text width) which 
#' is a fraction of the size of the text. Alternatives can be \code{"pw"} (page 
#' width), and \code{"in"}, \code{"cm"}, \code{"px"}.
#' @param reset Logical, default is \code{FALSE}. Whether arguments should be reset 
#' to default before applying modifications.
#'
#' @seealso
#' Exporting functions: \code{\link[fplot]{pdf_fit}}, \code{\link[fplot]{png_fit}}. 
#' The function closing the connection and showing the obtained graph in the viewer: 
#' \code{\link[fplot]{fit.off}}.
#' 
#' @details 
#' This function sets the option "fplot_export_opts" after parsing the arguments. 
#' This option is then automatically accessed by the functions used to export graphs
#' [export_graph_start()]. 
#' 
#' @return 
#' The function `setFplot_page()` does not return anything. It sets an 
#' R option containing the page parameters.
#' 
#' The function `getFplot_page()` returns the named list of page parameters which has been set  
#' in `setFplot_page()`.
#'
#'
#' @examples
#'
#' #
#' # How to set the page size
#' #
#'
#' # All examples below provide the same page size
#' setFplot_page(page = "us")
#' setFplot_page(page = "8.5in, 11in")
#' setFplot_page(page = "8.5/11in")
#' setFplot_page(page = c(8.5, 11))
#'
#' # All examples below provide the same margins
#' setFplot_page(margins = "normal")
#' setFplot_page(margins = "2cm, 2.5cm, 2cm, 2.5cm")
#' setFplot_page(margins = "2/2.5/2/2.5cm")
#' setFplot_page(margins = c(2, 2.5) / 2.54) # cm to in
#' setFplot_page(margins = c(2, 2.5, 2, 2.5) / 2.54)
#'
setFplot_page = function(page = "us", margins = "normal", units = "tw", pt = 10, w2h = 1.75, reset = FALSE){

    arg_list = c("units", "pt", "w2h")
    # page and margins => different behavior

    # page => us / a4 / a3 / a2 / a1 // => to be implemented later
    # or: w, h (vector or character)

    # margins: normal / thin
    # or vector of length 1 / 2 or 4
    # or b, l, t, r with in/cm

    # Later => add default for width, height, w2h etc

    check_arg_plus(page, "match(us, a4) | vector character len(1) | vector numeric len(,2) GT{0}")
    # check_arg_plus(page, "match(us, a4, beamer) | vector character len(1) | vector numeric len(,2) GT{0}")
    check_arg(units, "charin(tw, pw, in, cm, px)")
    check_arg(pt, "numeric scalar GT{0}")
    check_arg(w2h, "numeric scalar GT{0}")
    check_arg(reset, "logical scalar")

    is_px = FALSE
    if(length(page) == 1){
        if(is.numeric(page)){
            page_dim = rep(page, 2)
        } else if(page == "us"){
            page_dim = c(8.5, 11)
        } else if(page == "a4"){
            page_dim = c(8.3, 11.7)
        }  else {
            is_px = grepl("px", page, fixed = TRUE)
            page_dim = get_dimensions(page, 2)
        }
    } else {
        page_dim = page
    }

    check_arg_plus(margins, "match(normal, thin, FALSE, F, 0) | vector character len(1) | vector numeric len(,2) GE{0} | vector numeric len(4) GE{0}")

    if(length(margins) == 1){
        if(is.numeric(margins)){
            mar = rep(margins, 4)
        } else if(margins %in% c("FALSE", "F", "0")){
            mar = rep(0, 4)
        } else if(margins == "normal"){
            mar = c(2, 2.5, 2, 2.5) / 2.54
        } else if(margins == "thin"){
            mar = c(1.5, 1, 1.5, 1) / 2.54
        } else {
            # valid measures: in, cm, px
            mar = get_dimensions(margins, 4)
        }
    } else if(length(margins) == 2){
        mar = c(margins, margins)
    } else {
        # length = 4 => ok
        mar = margins
    }

    # page_dim_net = page_dim - c(sum(mar[c(2, 4)]), sum(mar[c(1, 3)]))

    opts = getOption("fplot_export_opts")
    if(is.null(opts) || reset){
        opts = list()
    } else if(!is.list(opts)){
        warning("Wrong formatting of option 'fplot_export_opts', all options are reset.")
        opts = list()
    }

    mc = match.call()
    args2set = unique(c(intersect(names(mc), arg_list), setdiff(arg_list, names(opts))))

    # NOTA: we don't allow delayed evaluation => all arguments must have hard values
    for(v in args2set){
        opts[[v]] = eval(as.name(v))
    }

    # Setting the pages dimensions
    if("margins" %in% names(mc) || !"margins" %in% names(opts)){
        # If user provided or no default
        opts$mar = mar
    } else {
        # else: default
        mar = opts$mar
    }

    if("page" %in% names(mc) || !"page_dim" %in% names(opts)){
        # if user provided page or no default
        opts$page_dim = page_dim
        opts$is_px = is_px
    } else {
        # page size = default
        page_dim = opts$page_dim
    }

    opts$page_dim_net = page_dim - c(sum(mar[c(2, 4)]), sum(mar[c(1, 3)]))

    options(fplot_export_opts = opts)

}

#' @rdname setFplot_page
getFplot_page = function(){
    opts = getOption("fplot_export_opts")
    if(is.null(opts) || !is.list(opts)){
        setFplot_page()
        opts = getOption("fplot_export_opts")
    }
    opts
}

#' PDF export with guaranteed text size
#' 
#' (*This function is deprecated: Please use the functions [export_graph_start()] 
#' and [export_graph_end()] instead.*) 
#' This function is an alternative to \code{\link[grDevices]{pdf}}, it makes it easy 
#' to export figures of appropriate size that should end up in a document. Instead 
#' of providing the height and width of the figure, you provide the fraction of the 
#' text-width the figure should take, and the target font-size at which the plotting 
#' text should be rendered. The size of the plotting text, once the figure is 
#' in the final document, is guaranteed.
#'
#' @param file The name of the file to which export the figure.
#' @param pt The size of the text, in pt, once the figure is inserted in your final document. 
#' The default is 10. This means that all text appearing in the plot with `cex = 1`
#'  will appear with 10pt-sized fonts in your document.
#' @param width The width of the graph, expressed in percentage of the width of 
#' the body-text of the document in which it will be inserted. Default is 1, which means 
#' that the graph will take 100% of the text width. It can also be equal to a character 
#' of the type `"100%"` or `"80%"`. Alternatively, the following units 
#' are valid. Relative sizes: `"pw"` (page width), `"tw"` (text width), 
#' `"ph"` (page height), `"th"` (text height). 
#' Absolute sizes: `"in"`, `"cm"`, and `"px"`.
#' @param height Numeric between 0 and 1 or character scalar. The height of the graph, 
#' expressed in percentage of the height of the body-text of the document in which it 
#' will be inserted. Default is missing, and the height is determined by the other 
#' argument `w2h`. This argument should range between 0 and 1. It can also be 
#' equal to a character of the type `"100%"` or `"80%"`. Alternatively, the 
#' following units are valid. Relative sizes: `"pw"` (page width), `"tw"` 
#' (text width), `"ph"` (page height), `"th"` (text height). Absolute 
#' sizes: `"in"`, `"cm"`, and `"px"`.
#' @param w2h Numeric scalar. Used to determine the height of the figure based on 
#' the width. By default it is equal to `1.75` which means that the graph
#'  will be 1.75 larger than tall. Note that when argument `sideways = TRUE`, 
#' the default for the height becomes `90%`.
#' @param h2w Numeric scalar, default is missing. Used to determine the aspectr ratio of the figure.
#' @param sideways Logical, defaults to `FALSE`. If the figure will be placed in 
#' landscape in the final document, then `sideways` should be equal to `TRUE`. 
#' If TRUE, then the argument `width` now refers to the height of the text, and the
#'  argument `height` to its width.
#' @param ... Other arguments to be passed to \code{\link[grDevices]{pdf}}.
#'
#' @details
#' If you use \code{\link[fplot]{fit.off}} instead of `dev.off` to close the graph, 
#' the resulting graph will be displayed in the viewer pane. So you don't have to open 
#' the document to see how it looks.
#' 
#' To export a ggplot2 graph, remember that you need to **print** it!
#' 
#' ```
#' library(ggplot2)
#' data = data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
#' 
#' # NOT GOOD
#' pdf_fit("test.pdf")
#' ggplot(data, aes(x, y)) +
#'   geom_point(color = "#54BF98") +
#'   geom_line(color = "#d34661")
#' fit.off()
#' 
#' # GOOD
#' my_graph = ggplot(data, aes(x, y)) +
#'              geom_point(color = "#54BF98") +
#'              geom_line(color = "#d34661")
#' 
#' pdf_fit("test.pdf")
#' print(my_graph)
#' fit.off()
#' ```
#' 
#' @return 
#' This function does not return anything. It connects the output of the R graphics
#' engine to a file. 
#'
#' @section Setting the page size:
#'
#' You can set the page size with the function \code{\link[fplot]{setFplot_page}}, 
#' which defines the size of the page and its margins to deduce the size of the body 
#' of the text in which the figures will be inserted. By default the page is considered 
#' to be US-letter with *normal* margins (not too big nor thin).
#'
#' It is important to set the page size appropriately to have a final plotting-text size 
#' guaranteed once the figure is inserted in the document.
#'
#' @seealso
#' To set the geometry and the defaults: \code{\link[fplot]{setFplot_page}}. 
#' To close the graph and display it on the viewer pane: \code{\link[fplot]{fit.off}}.
#'
#' @author
#' Laurent Berge
#'
#' @examples
#'
#'
#' # This function creates figures made to be inserted
#' # in a Latex document (US-letter with "normal" margins)
#' # By default, the figures should take 100% of the
#' # text width. If so, the size of the text in the figures
#' # will be exact.
#'
#' # You need pdftools and knitr to display PDFs in the viewer pane with fit.off
#' if(require(pdftools) && require(knitr)){
#'
#'   tmpFile = file.path(tempdir(), "pdf_examples.pdf")
#'
#'   pdf_fit(tmpFile, pt = 8)
#'   plot(1, 1, type = "n", ann = FALSE)
#'   text(1, 1, "This text will be displayed in 8pt.")
#'   fit.off()
#'
#'   pdf_fit(tmpFile, pt = 12)
#'   plot(1, 1, type = "n", ann = FALSE)
#'   text(1, 1, "This text will be displayed in 12pt.")
#'   fit.off()
#'
#'   pdf_fit(tmpFile, pt = 12, sideways = TRUE)
#'   plot(1, 1, type = "n", ann = FALSE)
#'   text(1, 1, "This text will be displayed in 12pt if in sideways.")
#'   fit.off()
#'
#'   # If we reduce the end plot width but keep font size constant
#'   # this will lead to a very big font as compared to the plot
#'   pdf_fit(tmpFile, pt = 8, width = "50%")
#'   plot(1, 1, type = "n", ann = FALSE)
#'   text(1, 1, "This text will be displayed in 8pt\nif in 50% of the text width.")
#'   fit.off()
#' }
#'
#'
#'
#'
#'
#'
pdf_fit = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, ...){

    mc = match.call()

    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, 
                    sideways = sideways, mc = mc, check_px = FALSE)

    pdf(file, width = opts$export_width, height = opts$export_height, pointsize = opts$pt, ...)
    options(fplot_export_path = file)
    options(fplot_export_type = "pdf")

}


#' PNG export with guaranteed text size
#'
#' (*This function is deprecated: Please use the functions [export_graph_start()] 
#' and [export_graph_end()] instead.*)  
#' This is an alternative to \code{\link[grDevices]{png}} and others. It makes it 
#' easy to export figures that should end up in documents. Instead of providing the
#'  height and width of the figure, you provide the fraction of the text-width the figure 
#' should take, and the target font-size at which the plotting text should be rendered. 
#' The size of the plotting text, once the figure is in the final document, is guaranteed.
#'
#' @inheritParams pdf_fit
#' @inheritSection pdf_fit Setting the page size
#'
#' @param res Numeric, the resolution in ppi. Default is 300.
#' @param ... Other arguments to be passed to \code{\link[grDevices:png]{bmp}}, 
#' \code{\link[grDevices]{png}}, \code{\link[grDevices:png]{jpeg}}, or 
#' \code{\link[grDevices:png]{tiff}}. For example: \code{antialias}, \code{bg}, etc.
#' 
#' @return 
#' This function does not return anything. It connects the output of the R graphics
#' engine to a file. 
#'
#'
#' @examples
#'
#'
#' # This function creates figures made to be inserted
#' # in a Latex document (US-letter with "normal" margins)
#' # By default, the figures should take 100% of the
#' # text width. If so, the size of the text in the figures
#' # will be exact.
#'
#' tmpFile = file.path(tempdir(), "png_examples.png")
#'
#' png_fit(tmpFile, pt = 8)
#' plot(1, 1, type = "n", ann = FALSE)
#' text(1, 1, "This text will be displayed in 8pt.")
#' fit.off()
#'
#' png_fit(tmpFile, pt = 12)
#' plot(1, 1, type = "n", ann = FALSE)
#' text(1, 1, "This text will be displayed in 12pt.")
#' fit.off()
#'
#' png_fit(tmpFile, pt = 12, sideways = TRUE)
#' plot(1, 1, type = "n", ann = FALSE)
#' text(1, 1, "This text will be displayed in 12pt if in sideways.")
#' fit.off()
#'
#' # If we reduce the end plot width but keep font size constant
#' # this will lead to a very big font as compared to the plot
#' png_fit(tmpFile, pt = 8, width = "50%")
#' plot(1, 1, type = "n", ann = FALSE)
#' text(1, 1, "This text will be displayed in 8pt\nif the graph is 50% of the text width.")
#' fit.off()
#'
png_fit = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, res = 300, ...){

    mc = match.call(expand.dots = TRUE)
    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, sideways = sideways, mc = mc)

    png(file, width = opts$export_width, height = opts$export_height, res = res, 
        units = opts$units, pointsize = opts$pt, ...)
    options(fplot_export_path = file)
    options(fplot_export_type = "png")
}


#' @rdname png_fit
tiff_fit = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, res = 300, ...){

    mc = match.call(expand.dots = TRUE)
    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, sideways = sideways, mc = mc)

    tiff(file, width = opts$export_width, height = opts$export_height, res = res, 
         units = opts$units, pointsize = opts$pt, ...)
    options(fplot_export_path = file)
    options(fplot_export_type = "tiff")
}

#' @rdname png_fit
jpeg_fit = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, res = 300, ...){

    mc = match.call(expand.dots = TRUE)
    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, sideways = sideways, mc = mc)

    jpeg(file, width = opts$export_width, height = opts$export_height, res = res, 
         units = opts$units, pointsize = opts$pt, ...)
    options(fplot_export_path = file)
    options(fplot_export_type = "jpeg")
}

#' @rdname png_fit
bmp_fit = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, res = 300, ...){

    mc = match.call(expand.dots = TRUE)
    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, sideways = sideways, mc = mc)

    bmp(file, width = opts$export_width, height = opts$export_height, res = res, 
        units = opts$units, pointsize = opts$pt, ...)
    options(fplot_export_path = file)
    options(fplot_export_type = "bmp")
}

# What follows is an internal function
fit_page = function(pt = 10, width = 1, height, w2h = 1.75, h2w, sideways = FALSE, mc, check_px = TRUE){

    set_up(1)
    check_arg(pt, "numeric scalar GT{0}")
    check_arg(w2h, h2w, "numeric scalar GT{0}")
    check_arg(width, height, "scalar(numeric, character) GT{0}")
    check_arg(sideways, "logical scalar")

    # We check the call for forbidden elements
    problems = intersect(c("filename", "units", "pointsize"), names(mc))
    if(length(problems) > 0){
        if(identical(problems, "pointsize")){
            stop_up("You cannot use the argument 'pointsize', use argument 'pt' instead.")
        }
        stop_up("You cannot use the argument", enumerate_items(problems, "s.or.quote"), ".")
    }

    is_given = function(x) x %in% names(mc) && !is.null(x)
    arg_in = sapply(c("width", "height", "w2h", "h2w"), is_given)

    if(sum(arg_in) > 2){
        qui_pblm = c("width", "height", "w2h", "h2w")[arg_in]
        stop_up("You cannot provide the arguments ", enumerate_items(qui_pblm, "quote"), 
                " at the same time. It's max two at a time.")
    }

    MISS_RATIO = sum(arg_in[3:4]) == 0

    # The dimension of the page + default
    opts = getOption("fplot_export_opts")
    if(is.null(opts)){
        # options not initialized => init
        setFplot_page()
        opts = getOption("fplot_export_opts")
    }
    page_dim_net = opts$page_dim_net
    page_dim = opts$page_dim
    if(!"pt" %in% names(mc)){
        pt = opts$pt
    }
    if(!"w2h" %in% names(mc)){
        w2h = opts$w2h
    }

    if(arg_in[4]){
        w2h = 1 / h2w
    }

    # Handling the parameters
    if(sideways && opts$units %in% c("tw", "pw")){
        if(missing(height) && MISS_RATIO){
            w2h = NULL
            height = 0.9
        } else if(!missing(height)){
            w2h = NULL
        }
        page_dim = rev(page_dim)
        page_dim_net = rev(page_dim_net)
    } else {
        if(!missing(height)){
            w2h = NULL
        }
    }

    width_in = get_dimensions(width, 1, opts$units, page_dim, page_dim_net)
    height_in = get_dimensions(height, 1, opts$units, page_dim, page_dim_net)

    if(is.null(w2h)){
        height_relative = height_in / width_in
    } else {
        height_relative = 1 / w2h
    }

    # # We find the optimal pdf output size
    # char_size_pt = par("cin")[2] * 72
    #
    # export_width = char_size_pt / pt * width_in
    # export_height =  height_relative * export_width
    # pt = 12

    # Using directly the argument pointsize is more reliable
    export_width = width_in
    export_height = height_relative * export_width

    if(check_px && opts$is_px){
        export_width = export_width * 96
        export_height = export_height * 96
        units = "px"
    } else {
        units = "in"
    }

    list(export_width = export_width, export_height = export_height, pt = pt, units = units)
}



#' Closes the current plotting device and shows the result in the viewer
#'
#' *This function is deprecated: Please use the functions [export_graph_start()] 
#' and [export_graph_end()] instead.*
#' 
#' To be used in combination with \code{\link[fplot]{pdf_fit}} or \code{\link[fplot]{png_fit}} 
#' when exporting images. It performs exactly the same thing as \code{dev.off()} but additionaly 
#' shows the resulting graph in the viewer pane provided you're using RStudio.
#'
#' @details
#' To view the results of PDF exports, the function \code{pdf_convert} from package \code{pdftools} 
#' is used to convert the PDF files into images -- so you need to have installed 
#' \code{pdftools} to make it work.
#'
#' In PDFs, only the first page will be viewed.
#'
#' @author
#' Laurent Berge
#'
#' @seealso
#' The tool to set the page size and the exporting defaults: \code{\link[fplot]{setFplot_page}}. 
#' Exporting functions \code{\link[fplot]{pdf_fit}}, \code{\link[fplot:pdf_fit]{png_fit}}, 
#' \code{\link[fplot:pdf_fit]{jpeg_fit}}.
#' 
#' The functions [export_graph_start()] and [export_graph_end()] provide similar features.
#' 
#' @return 
#' This function does not return anything in R. It closes the connection between the 
#' R graphics engine and a file that has been defined via one of the functions:
#' pdf_fitpng_fit
#'
#' @examples
#'
#' # Exportation example
#' # The functions pdf_fit, png_fit, etc, guarantee the right
#' #  point size of the texts present in the graph.
#' # But you must give the exact size the graph will take in your final document.
#' # => first use the function setFplot_page, default is:
#' # setFplot_page(page = "us", margins = "normal")
#' # By default the graph takes 100% of the text width
#'
#' data(us_pub_econ)
#'
#' tmpFile = file.path(tempdir(), "DISTR -- institutions.png")
#'
#' png_fit(tmpFile)
#' plot_distr(~institution, us_pub_econ)
#' fit.off()
#'
#' # What's the consequence of increasing the point size of the text?
#' png_fit(tmpFile, pt = 15)
#' plot_distr(~institution, us_pub_econ)
#' fit.off()
#'
#'
fit.off = function(){
    path = getOption("fplot_export_path")

    dev.off()

    my_viewer = getOption("viewer")

    if(is.null(my_viewer)){
        # => this setup to avoid repeating the warnings when running the examples
        old_time = getOption("fplot_export_warn_viewer")
        new_time = Sys.time()
        if(is.null(old_time) || difftime(new_time, old_time, units = "min") > 1){
            warning("The function 'fit.off' only works with RStudio's viewer--which wasn't found.")
            options(fplot_export_warn_viewer = new_time)
        }
    } else if(!is.null(path)){
        # We copy the image and show in the viewer
        tmpDir = tempdir()

        doView = TRUE
        target_path = file.path(tmpDir, "fplot_export_exported.PNG")

        export_type = getOption("fplot_export_type")
        if(export_type == "pdf"){
            if(!requireNamespace("pdftools", quietly = TRUE)){
                warning("To preview exported PDF files in the viewer, you need to install the package 'pdftools'.")
                doView = FALSE
                # Nothing is done
            } else {
                suppressWarnings(suppressMessages(pdftools::pdf_convert(path, page = 1, 
                                                                        filenames = target_path, 
                                                                        verbose = FALSE)))
            }
        } else {
            file.copy(path, target_path, overwrite = TRUE)
        }
        
        if(!requireNamespace("knitr", quietly = TRUE)){
            warning("To preview exported PDF files in the viewer, you need to install the package 'knitr'.")
            doView = FALSE
            # Nothing is done
        }

        if(doView){
            # setting up the html document
            # embedding the image is much more robust (fixes bug in VSCode)
            
            html_body = paste0("
<!DOCTYPE html>
<html> <body>
<img src = '", knitr::image_uri(target_path), "' alt='Exported image' width = '100%'>
</body> </html>\n")

            html_path = file.path(tmpDir, "fplot_export_html.html")
            writeLines(html_body, html_path)
            
            my_viewer(html_path)
        }

    }
}


get_dimensions = function(x, n_out, unit.default, page_dim, page_dim_net){
    # n_out: if n_out == 1: we want the width or the height
    # unit.default, page_dim, page_dim_net: only used when n_out = 1

    set_up(1 + (n_out == 1))

    arg_name = deparse(substitute(x))

    # valid measures: in, cm, px
    if(n_out == 1){
        if(missing(x)) return(NULL)

        if(arg_name == "width"){
            tw = page_dim_net[1]
            pw = page_dim[1]
        } else if(arg_name == "height"){
            tw = page_dim_net[2]
            pw = page_dim[2]
        } else {
            stop_up("Internal error: please contact the package author.")
        }

        if(is.numeric(x)){
            res = switch(unit.default, "tw" = x * tw, "pw" = x * pw, "in" = x, "cm" = x/2.54, "px" = x/96)

            return(res)
        }

        valid_units = c("tw", "pw", "th", "ph", "in", "cm", "px", "%")
    } else {
        valid_units = c("in", "cm", "px")
    }

    unit_all = sapply(valid_units, function(u) grepl(u, x, fixed = TRUE))
    if(sum(unit_all) == 0){
        stop_up("In argument '", arg_name, "', you must provide units. Valid units are ", enumerate_items(valid_units), ".")
    }

    if(sum(unit_all) > 1){
        stop_up("In argument '", arg_name, "', you cannot provide different units at the same time. You must choose between ", enumerate_items(valid_units), ".")
    }

    unit = valid_units[unit_all]

    m = strsplit(gsub("[[:alpha:]%]", "", x), ",|;|/")[[1]]
    m = trimws(m)
    # m = m[nchar(m) > 0]
    m = m[!grepl("^ *$", m) > 0]

    len_valid = switch(as.character(n_out), "1" = 1, "2" = 1:2, "4" = c(1, 2, 4))

    if(!length(m) %in% len_valid){
        stop_up("Problem in parsing the dimensions of argument '", arg_name, "': the number of elements is not valid. Please see the help on how to form it.")
    }

    m = tryCatch(as.numeric(m), warning = "problem")
    if(!is.numeric(m)){
        stop_up("Problem in parsing the dimensions of argument '", arg_name, "', conversion to numeric failed. Please see the help on how to form it.")
    }

    if(n_out == 1){
        res = m
    } else if(n_out == 2){
        res = switch(as.character(length(m)), "1" = c(m, 1.61*m), "2" = m)
    } else if(n_out == 4){
        res = switch(as.character(length(m)), "1" = rep(m, 4), "2" = rep(m, 2), "4" = m)
    }

    if(unit == "cm"){
        res = res / 2.54
    } else if(unit == "px"){
        res = res / 96
    }

    if(n_out == 1){
        if(unit == "%"){
            if(!unit.default %in% c("tw", "pw")){
                stop_up("You can define '", arg_name, "' as percentage only when the default unit is 'tw' (text width) or 'pw' (page width), which can be set in setFplot_page().")
            }

            res = switch(unit.default, "tw" = res/100 * page_dim_net, "pw" = res/100 * page_dim)

        } else if(unit %in% c("tw", "pw", "th", "ph")){
            res = switch(unit, "tw" = res * page_dim_net[1], "pw" = res * page_dim[1],
                         , "th" = res * page_dim_net[2], "ph" = res * page_dim[2])
        }
    }


    res
}

#' Graph export with garanteed text size
#' 
#' This function facilitates graph exportation by taking into account the final 
#' destination of the graph (typically a document) and allowing the user to use 
#' point size, an intuitive unit
#' in written documents, as the graph scaler. Once located in the final document, the text of the graph
#' at the default size will be at the defined point size.
#' 
#' @inheritParams png_fit
#' @param file Character scalar. The name of the file in which to save the graph.
#' If the argument type is `NULL`, the type of file is deduced from the extension.
#' If your file extension is different from your file type, you need to use the 
#' argument `type`.
#' @param type Character scalar, default is `NULL`. The type of file to be created.
#' If `NULL`, the default, then the type of file is deduced from the extension.
#' 
#' @details 
#' 
#' To export a ggplot2 graph, remember that you need to **print** it!
#' 
#' ```
#' library(ggplot2)
#' data = data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
#' 
#' # NOT GOOD
#' export_graph_start("test.pdf")
#' ggplot(data, aes(x, y)) +
#'   geom_point(color = "#54BF98") +
#'   geom_line(color = "#d34661")
#' export_graph_end()
#' 
#' # GOOD
#' my_graph = ggplot(data, aes(x, y)) +
#'              geom_point(color = "#54BF98") +
#'              geom_line(color = "#d34661")
#' 
#' export_graph_start("test.pdf")
#' print(my_graph)
#' export_graph_end()
#' ```
#' 
#' When the function `export_graph_end()` is called, the resulting exported graph 
#' is displayed in the Viewer. The viewer function is found with 
#' `getOption("viewer")` and should work on RStudio and VSCode (with the R extension). 
#' 
#' @return 
#' These functions do not return anything in R. `export_graph_start` creates a
#' file linked to the R graphics engine, in which subsequent plots are saved.  
#' `export_graph_end` closes the connection and the file.
#' 
#' @inheritSection pdf_fit Setting the page size
#' 
#' @inherit fit.off seealso
#' 
#' @author 
#' Laurent Berge
#' 
#' @examples 
#' 
#' tmpFile = file.path(tempdir(), "png_examples.pdf")
#'
#' # we start the exportation
#' export_graph_start(tmpFile, pt = 8)
#' 
#' plot(1, 1, type = "n", ann = FALSE)
#' text(1, 1, "This text will be displayed in 8pt.")
#' 
#' # the line below closes the connection and displays the 
#' # graph in the viewer pane if appropritate
#' export_graph_end()
#' 
export_graph_start = function(file, pt = 10, width = 1, height, w2h = 1.75, h2w, 
                        sideways = FALSE, res = 300, type = NULL, ...){

    mc = match.call()
    
    check_arg(file, "character scalar mbt")
    check_arg(type, "NULL character scalar")
    
    if(is.null(type)){
        if(!grepl(".", file, fixed = TRUE)){
            stop("If argument 'type = NULL', the export type is deduced from the file extension.",
                 "\nPROBLEM: the file name does not contain an extension.")
        }
        type_raw = gsub(".+\\.", "", file)
        type = tolower(type_raw)
        accepted_types = c("pdf", "jpg", "jpeg", "png", "tiff", "bmp")
        if(!type %in% accepted_types){
            stop("If argument 'type = NULL', the export type is deduced from the file extension.",
                 "\nPROBLEM: the extension found, `", type_raw, "` is not valid.",
                 "\nFYI: the accepted types are: ", enumerate_items(accepted_types), ".")
        }
    } else {
        check_arg_plus(type, "match(pdf, jpg, jpeg, png, tiff, bmp)")
    }
    # here type is lowercase and an accepted extension
    if(type == "jpg") type = "jpeg"

    opts = fit_page(pt = pt, width = width, height = height, w2h = w2h, h2w = h2w, 
                    sideways = sideways, mc = mc, check_px = type != "pdf")
    
    if(type == "pdf"){
        pdf(file, width = opts$export_width, height = opts$export_height, pointsize = opts$pt, ...)
    } else {
        fun = switch(type, 
                     "png"  = grDevices::png,
                     "jpeg" = grDevices::jpeg,
                     "tiff" = grDevices::tiff,
                     "bmp"  = grDevices::bmp)
        
        fun(file, width = opts$export_width, height = opts$export_height, res = res, 
            units = opts$units, pointsize = opts$pt, ...)
    }
    
    options(fplot_export_path = file)
    options(fplot_export_type = type)    
}


#' @describeIn export_graph_start Ends the connection to the current export and creates the file.
export_graph_end = fit.off


####
#### Main Graph. Tools ####
####


extract_df = function(fml, df){
    # fml: one sided formula
    # df: data.frame containing the data

    if(is.null(fml)) return(NULL)

    is_intercept = grepl("(?<=( |~))1(?=( |$))", deparse(fml), perl = TRUE)
    if(is_intercept){
        # We keep the order provided by the user
        fml_str = gsub("(?<=( |~))1(?=( |$))", "Frequency", deparse(fml), perl = TRUE)
        fml = as.formula(fml_str)
        df[["Frequency"]] = 1
    }

    t = terms(fml)
    vars = attr(t, "term.labels")

    res = list()
    for(v in vars) res[[v]] = eval(parse(text = v), df)

    res
}


truncate_string = function(x, trunc = 20, method = "auto"){

    check_arg_plus(x, "vector character conv")
    check_arg(trunc, "integer scalar GE{3}")
    check_arg_plus(method, "match(auto, right, mid)")

    if(is.numeric(x)) x = as.character(x)

    n_all = nchar(x)

    if(method == "right"){
        res = substr(x, 1, trunc)
        qui = nchar(res) == trunc & n_all > trunc
        res[qui] = gsub("..$", "\\.\\.", res[qui])

    } else if(method == "mid"){
        res = x
        qui = n_all > trunc
        if(any(qui)){
            res_long = res[qui]
            n_long = n_all[qui]

            nfirst = ceiling(trunc / 2)
            nlast = trunc - nfirst
            res_new = paste0(substr(res_long, 1, nfirst-1), "...", substr(res_long, n_long - nlast + 1, n_long))
            res[qui] = res_new
        }

    } else {

        res = as.vector(sapply(x, cpp_string_shorten, max_size = as.integer(trunc)))

        # if the word exists => we trim the word
        # res = x
        # qui = n_all > trunc
        # while(any(qui)){
        #     res_long = res[qui]
        #
        #     res_long_split = strsplit2df(res_long, id = (1:length(x))[qui], split = " ")
        #     res_long_split[, n_char := nchar(x)]
        #
        #     # points don't count as characters
        #     # 1) drop stopwords (of, the, and)
        #     # 2) reduce non name words
        #     # 3) reduce name words
        #     # 4) trim right
        #
        #     # stopwords
        #     res_long_split = res_long_split[!x %in% c("the", "of", "and")]
        #     total = res_long_split[, list(n_char = sum(n_char) + .N - 1), by = id]
        #
        #     qui_ok = total$n_char <= trunc
        #     if(any(qui_ok)){
        #         id_ok = total$id[qui_ok]
        #         res[id_ok] = paste_conditional(x~id, res_long_split[id %in% id_ok])$x
        #         res_long_split = res_long_split[!id %in% id_ok]
        #         if(nrow(res_long_split) == 0) break
        #     }
        #
        #     # non name words
        #     res_long_split[, isEnglish := check_english(tolower(x), addNames = FALSE)]
        #     res_long_split[n_char > 5 & isEnglish == TRUE, x := paste0(substr(x, 1, 4), ".")]
        #     total = res_long_split[, list(n_char = sum(nchar(gsub(".", "", x, fixed = TRUE))) + .N - 1), by = id]
        #
        #     qui_ok = total$n_char <= trunc
        #     if(any(qui_ok)){
        #         id_ok = total$id[qui_ok]
        #         res[id_ok] = paste_conditional(x~id, res_long_split[id %in% id_ok])$x
        #         res_long_split = res_long_split[!id %in% id_ok]
        #         if(nrow(res_long_split) == 0) break
        #     }
        #
        #     # name words
        #     res_long_split[, isEnglishName := check_english(tolower(x)) & !isEnglish]
        #     res_long_split[n_char > 5 & isEnglishName == TRUE, x := paste0(substr(x, 1, 4), ".")]
        #     total = res_long_split[, list(n_char = sum(nchar(gsub(".", "", x, fixed = TRUE))) + .N - 1), by = id]
        #
        #     qui_ok = total$n_char <= trunc
        #     if(any(qui_ok)){
        #         id_ok = total$id[qui_ok]
        #         res[id_ok] = paste_conditional(x~id, res_long_split[id %in% id_ok])$x
        #         res_long_split = res_long_split[!id %in% id_ok]
        #         if(nrow(res_long_split) == 0) break
        #     }
        #
        #     # other words
        #     res_long_split[n_char > 5 & isEnglishName == FALSE & isEnglish == FALSE, x := paste0(substr(x, 1, 5), ".")]
        #     total = res_long_split[, list(n_char = sum(nchar(gsub(".", "", x, fixed = TRUE))) + .N - 1), by = id]
        #
        #     qui_ok = total$n_char <= trunc
        #     if(any(qui_ok)){
        #         id_ok = total$id[qui_ok]
        #         res[id_ok] = paste_conditional(x~id, res_long_split[id %in% id_ok])$x
        #         res_long_split = res_long_split[!id %in% id_ok]
        #         if(nrow(res_long_split) == 0) break
        #     }
        #
        #     # Now we trim on the right
        #     base_left = paste_conditional(x~id, res_long_split)
        #     id_left = base_left$id
        #     string_left = base_left$x
        #     res[id_left] = paste0(substr(string_left, 1, trunc), "...")
        #     break
        # }
    }

    res
}



legendFit = function(where = "top", legend, minCex = 0.7, trunc, trunc.method = "auto", plot = TRUE, title = NULL, title_out = FALSE, ...){
    # units in inch to avoid the need of having a graph already plotted
    # (you cannot use par("usr) when there is no graph plotted)
    # title_out: veut dire que le titre peut aller au dela de la plotting box

    # the title
    # check_arg_plus(title, "null character scalar conv")
    check_arg(title_out, "logical scalar")
    ADD_TITLE = FALSE
    if(length(title) == 1 && nchar(title) > 0 && grepl("[^ ]", title)){
        ADD_TITLE = TRUE
    }
    # decalage vers le bas de la legende (ssi title_out = TRUE)
    do_adj = 0
    if(!ADD_TITLE){
        do_adj = -1
    } else if(!title_out){
        do_adj = 1.9
    }
    # do_adj = ADD_TITLE && !title_out

    # 1) Truncation of the items
    n = length(legend)

    AUTO_TRUNC = TRUE
    if(!missing(trunc)){
        AUTO_TRUNC = FALSE
        if(is.logical(legend)) legend = as.character(legend)
        myLabels = truncate_string(legend, trunc, trunc.method)
    } else {
        myLabels = legend
        trunc = 100
    }

    # 2) finding the right cex
    largeur_totale = par("pin")[1]
    myCex = 1
    # other stuff from the legend is equal to 4 characters
    fsize = function(x, cex) max(strwidth(x, units = "in", cex = cex)) + strwidth("WWWl", units = "in", cex = cex)
    unit_size = fsize(myLabels, 1)
    while(myCex >= minCex && n * unit_size > largeur_totale){
        myCex = myCex * 0.95
        unit_size = fsize(myLabels, myCex)
    }

    nlines = 1
    if(n * unit_size > largeur_totale){
        # Doesn't fit in one line, we redo the "cex" exercise!
        nlines = 2
        n_top = ceiling(n/2)
        myCex = 1
        unit_size = fsize(myLabels, myCex)
        while(myCex >= minCex && n_top * unit_size > largeur_totale){
            myCex = myCex * 0.95
            unit_size = fsize(myLabels, myCex)
        }
    }

    res = list(cex = myCex)
    hauteur_caractere = strheight("W", units = "in", cex = myCex)

    if(nlines == 2){
        res$total_height = (4 + do_adj/2 + 0.5)*hauteur_caractere
    } else {
        res$total_height = (2.5 + do_adj/2 + 0.5)*hauteur_caractere
    }

    # Auto truncation
    if(AUTO_TRUNC && myCex <= minCex){
        n_relevant = ifelse(nlines == 2, n_top, n)

        minTrunc = 5
        trunc = max(minTrunc, min(25, max(nchar(legend))) - 3)
        myLabels = truncate_string(legend, trunc, trunc.method)
        unit_size = fsize(myLabels, myCex)
        while(n_relevant * unit_size > largeur_totale && trunc > minTrunc){
            trunc = max(minTrunc, trunc - 3)
            myLabels = truncate_string(myLabels, trunc, trunc.method)
            unit_size = fsize(myLabels, myCex)
        }
    }
    res$trunc = trunc

    if(plot == FALSE){
        return(res)
    }

    # Adjustment of the title => we can use "usr" measures because graph already there
    if(do_adj != 0){
        h = strheight("W") / diff(get_y_lim())
        adj_title = do_adj * h / 2.1
    } else {
        adj_title = 0
    }

    if(nlines == 2){
        # info for the two lines fit
        leg1 = legend(where, myLabels[1:n_top], fill = "black", cex = myCex, horiz = TRUE, plot = FALSE, inset = adj_title)
        leg2 = legend(where, myLabels[(n_top+1):n], fill = "black", cex = myCex, horiz = TRUE, plot = FALSE)
        leg2_x = leg2$rect$left
        if(grepl("top", where)){
            leg2_y = leg1$rect$top - 1.5*strheight("W", units = "user", cex = myCex)
        } else {
            leg2_y = leg1$rect$top + 1.5*strheight("W", units = "user", cex = myCex)
        }

    }

    # Mandatory
    legend_options = list(...)
    legend_options$legend = as.character(myLabels)

    # optional
    listDefault(legend_options, "bty", "n")
    listDefault(legend_options, "horiz", TRUE)
    listDefault(legend_options, "cex", myCex)
    listDefault(legend_options, "x", where)

    if(nlines == 1){
        if(do_adj != 0){
            legend_options$inset = adj_title
        }

        do.call("legend", legend_options)
    } else {
        # First legend
        legend_options_1 = legend_options
        for(var in intersect(names(legend_options_1), c("fill", "lwd", "lty", "col", "pch", "angle", "density"))){
            val = legend_options_1[[var]]
            n_val = length(val)
            if(n_val > 1){
                if(n_val < n){ # recycling
                    val = rep(val, ceiling(n/n_val))
                }

                legend_options_1[[var]] = val[1:n_top]
            }
        }

        legend_options_1$legend = legend_options_1$legend[1:n_top]

        if(do_adj != 0){
            legend_options_1$inset = adj_title
        }

        do.call("legend", legend_options_1)

        # Second legend
        legend_options_2 = legend_options
        for(var in intersect(names(legend_options_2), c("fill", "lwd", "lty", "col", "pch", "angle", "density"))){
            val = legend_options_2[[var]]
            n_val = length(val)
            if(n_val > 1){
                if(n_val < n){ # recycling
                    val = rep(val, ceiling(n/n_val))
                }

                legend_options_2[[var]] = val[(n_top + 1):n]
            }
        }

        legend_options_2$x = leg2_x
        legend_options_2$y = leg2_y
        legend_options_2$legend = legend_options_2$legend[(n_top + 1):n]

        do.call("legend", legend_options_2)
    }

    if(ADD_TITLE){
        if(title_out){
            legend("top", legend = title, adj = c(0, -1), bty = "n", text.font = 3, xpd = TRUE)
        } else {
            legend("top", legend = title, adj = c(0, -0.3), bty = "n", text.font = 3)
        }

    }

    return(invisible(res))
}

range_plus = function(x, percent = 0){
    check_arg(x, "numeric vector mbt")
    check_arg(percent, "numeric scalar")

    r_x = range(x)
    width = diff(r_x)
    r_x + c(-1, 1) * width/2 * percent/100
}


formatAxisValue = function(x, d = 2, r = 0, type = "abbrev"){
    # This function formats values to be displayed in the x-axis
    # It transforms them into easily readable format

    check_arg(d, "integer scalar GE{1}")
    check_arg(r, "integer scalar GE{0}")
    check_arg_plus(type, "match(abbrev, plain, signif, equation)")


    formatAxisValue_single = function(x, d, r, type){

        if(is.na(x)) return(NA)

        s = sign(x)
        x_abs = abs(x)

        if(type == "abbrev"){
            if(x_abs < 1e4){
                res = as.character(mysignif(x_abs, d = d, r = r))
            } else if(x_abs < 1e6){
                res = paste0(mysignif(x_abs / 1e3, d, r), "K")
            } else if(x_abs < 1e9){
                res = paste0(mysignif(x_abs / 1e6, d, r), "M")
            } else if(x_abs < 1e12){
                res = paste0(mysignif(x_abs / 1e9, d, r), "B")
            } else if(x_abs < 1e15){
                res = paste0(mysignif(x_abs / 1e12, d, r), "T")
            } else {
                res = x_abs
            }

            res = paste0(ifelse(s < 0, "-", ""), res)
        } else if(type == "plain"){
            res = numberFormat(x, d=d, r=r)
        } else if(type == "signif"){
            res = mysignif(x, d=d, r=r)
        } else if(type == "equation"){
            if(x_abs < 1e4){
                if(s >= 0){
                    res = substitute(x_val, list(x_val = mysignif(x, d = d, r = r)))
                } else {
                    res = substitute(-x_val, list(x_val = mysignif(x_abs, d = d, r = r)))
                }
            } else {
                pow = floor(log10(x_abs))
                if(s >= 0){
                    res = substitute(x_val%*%10^pow, list(x_val = mysignif(x/10**pow, d=d, r=r), pow=pow))
                } else {
                    res = substitute(-x_val%*%10^pow, list(x_val = mysignif(x_abs/10**pow, d=d, r=r), pow=pow))
                }
            }
        }

        res
    }


    res = sapply(x, formatAxisValue_single, d=d, r=r, type = type)

    if(length(x) == 1){
        res = res[[1]]
    }

    res
}


myBox = function(id){
    # This functions draws the box of the plot region
    # the id stands for which border to draw

    id = as.character(id)

    coords = par("usr")

    if(grepl("1", id)){
        axis(1, at = coords[1:2], lwd = 1, lwd.ticks = 0, labels = NA)
    }

    if(grepl("2", id)){
        axis(2, at = coords[3:4], lwd = 1, lwd.ticks = 0, labels = NA)
    }

    if(grepl("3", id)){
        axis(3, at = coords[1:2], lwd = 1, lwd.ticks = 0, labels = NA)
    }

    if(grepl("4", id)){
        axis(4, at = coords[3:4], lwd = 1, lwd.ticks = 0, labels = NA)
    }

}

shade_area <- function(y1, y2, x, xmin, xmax, col="grey", ...){
    # fonction plus pratique que polygon
    # elle permet de griser une partie d?limit?e par
    # y1 et y2 pour chacune des valeurs de x
    # on doit avoir la m?me longueur de y1,y2 et x
    # exemple:
    # a=curve(x**2,-5,5)
    # shade_area(a$y+1,a$y-1,a$x)
    # qqes parametres graphiques:
    # lwd / border (couleur du bord, peut etre NA) / lty

    n <- length(x)
    stopifnot(length(y1)==n | length(y1)==1)
    stopifnot(length(y2)==n | length(y2)==1)

    if(length(y1)==1) y1 <- rep(y1,n)
    if(length(y2)==1) y2 <- rep(y2,n)

    if(missing(xmin)) xmin <- min(x)
    if(missing(xmax)) xmax <- max(x)

    ind <- which(x>=xmin & x<=xmax)
    x1 <- x[ind] ; x2 <- x[rev(ind)]
    polygon(c(x1,x2),c(y1[ind],y2[rev(ind)]),col=col,...)
}



abplot <- function(x, y, where="default", signifCode = c("***" = 0.001, "**" = 0.05, "*" = 0.10), log = FALSE, legend=TRUE, ...){
    #plot a graph with the linear fit
    #where: where to place the legend

    # we take care of xlabs
    dots <- list(...)
    if(is.null(dots$xlab)){
        dots$xlab = deparse(substitute(x))
        if(log) dots$xlab = paste0("ln(", dots$xlab, ")")
    }

    if(is.null(dots$ylab)){
        dots$ylab = deparse(substitute(y))
        if(log) dots$ylab = paste0("ln(", dots$ylab, ")")
    }


    if(log){

        qui = which(x<=0 | y<=0)
        if(length(qui)>0){
            warning(length(qui), " observations were omitted because of the log transformation.", call. = FALSE, immediate. = TRUE)
            x = x[-qui]
            y = y[-qui]
        }

        x = base::log(x)
        y = base::log(y)
    }

    dots$x = x
    dots$y = y

    do.call(plot, dots)

    r = lm(y~x)
    abline(r)
    a = r$coefficients[1]
    b = r$coefficients[2]
    s = summary(r)
    r2 = s$r.squared
    pval = s$coefficients[2,4]
    star = as.character(cut(pval, breaks = c(-1, signifCode, 100), labels = c(names(signifCode), "n.s.")))


    # setting the default of 'where'
    if(where == "default"){
        where = tell_me_where(x, y)
    }

    # the text to be displayed
    if(log){
        if(b<0){
            myEq = substitute(widehat(ln(Y))==a-b%*%ln(X)^s~~~~phantom(0), list(a=signif(a, 2), b=-(signif(b, 2)), s=star))
        } else {
            myEq = substitute(widehat(ln(Y))==a+b%*%ln(X)^s~~~~phantom(0), list(a=signif(a, 2), b=signif(b, 2), s=star))
        }
    } else {
        if(b<0){
            myEq = substitute(hat(Y)==a-b%*%X^s~~~~phantom(0), list(a=signif(a, 2), b=-(signif(b, 2)), s=star))
        } else {
            myEq = substitute(hat(Y)==a+b%*%X^s~~~~phantom(0), list(a=signif(a, 2), b=signif(b, 2), s=star))
        }
    }

    if(legend) legend(where,legend = c(myEq, substitute(R^2 == r2, list(r2=signif(r2, 2))), expression()),  cex=.8, bty="n")
}



tell_me_where = function(x, y, all=FALSE){
    # This function tells, given a cloud of data points,
    # where to put the legend

    # Clean NAs =>
    whoIsNA = which(is.na(y) | is.na(x))
    if(length(whoIsNA)>0){
        x = x[-whoIsNA]
        y = y[-whoIsNA]
    }

    x_mid = min(x) + diff(range(x))/2
    y_mid = min(y) + diff(range(y))/2

    square_left_bottom= sum(x<=x_mid & y<=y_mid)
    square_right_bottom = sum(x>=x_mid & y<=y_mid)

    square_left_top= sum(x<=x_mid & y>=y_mid)
    square_right_top = sum(x>=x_mid & y>=y_mid)

    where_id = order(c(square_left_bottom, square_right_bottom, square_left_top, square_right_top))
    where_name = c("bottomleft", "bottomright", "topleft", "topright")

    if(all){
        where = where_name[where_id]
    } else {
        where = where_name[where_id][1]
    }

    return(where)
}


hgrid = function(lty = 3, col = "darkgray", ymin = -Inf, ymax = Inf, ...){
    # simple function that draws an horizontal grid

    check_arg(ymin, ymax, "numeric scalar")

    # Finding the coordinates
    y = axis(2, lwd=0, labels = NA)

    y = y[y > ymin & y < ymax]

    # now drawing the lines
    if(length(y) > 0){
        abline(h = y, col = col, lty = lty, ...)
    }
}


vgrid = function(lty = 3, col = "darkgray", ymin = -Inf, ymax = Inf, ...){
    # simple function that draws a vertical grid

    check_arg(ymin, ymax, "numeric scalar")

    # Finding the coordinates
    x = axis(1, lwd=0, labels = NA)

    # Should we trim?
    y_range = par("usr")[3:4]
    if(ymin > y_range[1] || ymax < y_range[2]){
        segments(x0 = x, y0 = max(ymin, y_range[1]), x1 = x, y1 = min(ymax, y_range[2]),
                 col = col, lty = lty, ...)
    } else {
        # now drawing the lines
        abline(v = x, col = col, lty = lty, ...)
    }

}


get_y_lim = function(){
    myRawLim = par("usr")
    # we dis-extend by 4 percent at each end
    xrange = myRawLim[2] - myRawLim[1]
    x_ext = 0.04 * (xrange / 1.08)
    yrange = myRawLim[4] - myRawLim[3]
    y_ext = 0.04 * (yrange / 1.08)

    myLim = myRawLim + c(x_ext, -x_ext, y_ext, -y_ext)
    y_min = myLim[3]
    y_max = myLim[4]

    isLog = par("ylog")
    if(isLog){
        res = 10 ** c(y_min, y_max)
    } else {
        res = c(y_min, y_max)
    }

    return(res)
}



get_x_lim = function(){
    myRawLim = par("usr")
    # we dis-extend by 4 percent at each end
    xrange = myRawLim[2] - myRawLim[1]
    x_ext = 0.04 * (xrange / 1.08)
    yrange = myRawLim[4] - myRawLim[3]
    y_ext = 0.04 * (yrange / 1.08)

    myLim = myRawLim + c(x_ext, -x_ext, y_ext, -y_ext)
    x_min = myLim[1]
    x_max = myLim[2]

    isLog = par("xlog")
    if(isLog){
        res = 10 ** c(x_min, x_max)
    } else {
        res = c(x_min, x_max)
    }

    return(res)
}

usr_width = function(xlim){
    # we add 4% both sides
    x_range = abs(diff(xlim))
    # xlim + x_range * 0.04 * c(-1, 1)
    x_range + 2 * 0.04 * x_range
}


get_right_coordinates = function(y, ylim_left = NULL, ylim_right){
    # We transform the coordinates!

    # if the limits are not given => we find it!

    if(is.null(ylim_left)){
        ylim = get_y_lim()
    } else {
        ylim = ylim_left
    }

    y_min = ylim[1]
    y_max = ylim[2]

    return(y_new = to01(y, ylim_right)*(y_max-y_min) + y_min)
}

rightLine = function(x, y, ylim = NULL, ylim_left = NULL, showAxis = TRUE, showPoints = TRUE, showLine = TRUE, nb.signif = 2, ...){
    # We draw a line of a different scale on an existing plot.
    # We put the axis on the right

    y_new = get_right_coordinates(y, ylim_left, ylim_right = ylim)

    dots = list(...)
    listDefault(dots, "lty", 2)
    listDefault(dots, "col", 2)
    listDefault(dots, "pch", 2)
    dots$x = x
    dots$y = y_new

    if(showLine) do.call(lines, dots)
    if(showPoints) do.call(points, dots)

    if(showAxis){
        if(is.null(ylim_left)) ylim = get_y_lim()

        y_min = ylim[1]
        y_max = ylim[2]

        y_at = axis(2, lwd=0)
        axis(4, at = y_at, labels = signif((y_at-y_min) / (y_max-y_min) * (max(y) - min(y)) + min(y), nb.signif), col.axis=dots$col)
    }

    invisible(y_new)
}


drawRectangle = function(xbl, ybl, xtr, ytr, prop=1, coul=1:100, sep=0.02, ...){
    # on donne a la fonction:
    # - le (x,y) en bas a gauche du rectangle (bl: bottm left)
    # - le (x,y) en haut a droite du rectangle (tr: top right)
    # - prop: le vecteur des proportions (on peut decouper le rectangle en plusieurs parties)
    # - le vecteur des couleurs a utiliser

    # The real function
    prop_bis = prop*(xtr-xbl-2*sep)

    X = xbl + cumsum(c(sep,prop_bis))

    n = length(X)
    start = X[-n]
    end = X[-1]
    for(i in 1:n){
        rect(xleft=start[i], xright=end[i], ybottom=ybl+sep, ytop=ytr-sep, col=coul[i], ...)
    }

}


myHist = function(x, maxValue = +Inf, cex.text = 0.7, doubleTable = FALSE, log = FALSE, use_xaxis, inCol = "#386CB0", outCol = "white",  ...){
    # personalized histogram

    if(doubleTable){
        tx = table_collapse(x)
    } else {
        tx = round(x)
    }

    if(log){
        tx = floor(base::log(tx + 1e-6))
    }

    if(length(unique(tx)) > 500){
        stop("There is more than 500 categories!!! Reduce the data!")
    }


    if(any(tx>maxValue)){
        overMax = TRUE
    } else {
        overMax = FALSE
    }

    tx[tx>maxValue] = maxValue

    ttx = table_collapse(tx)

    if(overMax & !log) names(ttx)[length(ttx)] = paste0(maxValue,"+")

    # New version
    dots = list(...)
    dots$axes = FALSE
    dots$axisnames = FALSE
    dots$col = 0
    dots$border = FALSE

    useAxis = FALSE
    if(!missing(use_xaxis)){
        if(!all(c("log", "all_names") %in% names(use_xaxis))) stop("You must give a myHist object in argument use_xaxis.")
        if(xor(log, use_xaxis$log)) stop("The 'log' status must be identical to the one in use_xaxis.")
        # dots$xlim = use_xaxis$xlim
        useAxis = FALSE

        # We rework ttx share
        v = ttx
        myNames = union(names(v), use_xaxis$all_names)
        new_ttx = v[myNames]
        names(new_ttx) = myNames
        new_ttx[is.na(new_ttx)] = 0

        ttx = new_ttx
    }

    ttx_share = round(ttx/sum(ttx)*100, 1)

    if(is.null(dots$ylim)) {
        ylim = c(0, max(ttx_share))
        hauteur_caractere = strheight("W", "in")
        ylim[2] = ylim[2] + 2*hauteur_caractere / par("pin")[2] * diff(ylim)
        dots$ylim = ylim
    }

    dots$height = ttx_share

    info = do.call(barplot, dots)

    # we get the "nice" points display
    y_points = axis(2, lwd=0, col.axis = 0)
    abline(h=y_points[-1], lty=3, col="lightgrey")
    barplot(ttx_share, add = TRUE, axes = FALSE, ylim=ylim, axisnames = FALSE, col = inCol, border=outCol)
    axis(2, at=y_points, labels = paste0(y_points, "%"), las = 2)

    if(log){
        labels = round(exp(as.numeric(names(ttx_share))))[-1]

        axis(1, at = (info[-1] + info[-length(info)])/ 2, lwd = 0, lwd.ticks = 1, labels = labels)
    } else {
        labels = names(ttx_share)

        axis(1, at = info, lwd = 0, labels = labels)
        # xaxis_labels(at = info, labels = names(ttx), ...)
    }

    text(info, ttx_share, labels = addCommas(ttx), pos = 3, cex = cex.text)

    invisible(list(log=log, all_names = names(ttx_share)))
}


myBarplot = function(x, order=FALSE, nbins=10, show0=TRUE, cex.text=0.7, isLog=FALSE, isDistribution = TRUE, yaxis.show = TRUE, niceLabels = FALSE, labels.tilted=FALSE, axis1Opts = list(), hgrid = TRUE, top = "nb", showOther = TRUE, inCol = "#386CB0", outCol = "white", trunc=20, trunc.method = "auto", line.max, ...){
    # This function draws a nice barplot

    # We get whether the labels from x are numeric
    isNumericLabel = is.numeric(tryCatch(as.numeric(names(x)), warning = function(x) "problem"))

    # we transform x if necessary
    if(!show0) x = x[x>0] # we don't show values 0
    if(order) x = sort(x, decreasing = TRUE)

    doTrim = FALSE
    if(length(x) > nbins) {

        if(!showOther){
            doTrim = TRUE
        } else {
            y = x[1:(nbins-1)]

            # We change the name
            max_name = ifelse(isNumericLabel & !order, paste0(names(x)[nbins], "+"), "other")
            allNames = names(y)

            # we recreate x
            z = c(y, sum(x[nbins:length(x)]))
            x = z
            names(x) = c(allNames, max_name)
        }

    }

    if(isDistribution){
        x_share = round(x/sum(x)*100, 5)
    } else {
        x_share = x
    }

    if(doTrim){
        cases_left = length(x) - nbins
        sum_x = sum(x)
        x = x[1:nbins]
        x_share = x_share[1:nbins]
        share_left = 1 - sum(x) / sum_x
    }

    # New version
    dots_1st = list(...)
    dots_1st$axes = FALSE
    dots_1st$axisnames = FALSE
    dots_1st$col = 0
    dots_1st$border = 0
    dots_1st$height = x_share

    if(!"ylim" %in% names(dots_1st)){
        ylim = c(0, max(x_share))

        hauteur_top = 0
        if(top != "none"){
            hauteur_top = strheight("W", "in")
        }

        hauteur_missing = 0
        if(doTrim){
            hauteur_missing = strheight("W", "in")
        }

        ylim[2] = ylim[2] + (2*hauteur_top + 2.5*hauteur_missing) / par("pin")[2] * diff(ylim)

        dots_1st$ylim = ylim

    }
    info = do.call("barplot", dots_1st)

    if(yaxis.show){
        # we get the "nice" points display
        y_points = axis(2, lwd=0, col.axis = 0)
        if(isDistribution){
            axis(2, at=y_points, labels = paste0(y_points, "%"), las = 2)
        } else {
            axis(2, at=y_points, labels = y_points, las = 2)
        }

        if(hgrid) abline(h=y_points[-1], lty=3, col="gray")

    }

    # now the "real" plot
    dots = list(...)
    dots$add = TRUE
    dots$axes = FALSE
    dots$axisnames = FALSE
    dots$col = inCol
    dots$border = outCol
    dots$height = x_share
    do.call("barplot", dots)

    funLabels = ifelse(labels.tilted, "xaxis_biased", "xaxis_labels")

    if(missnull(line.max)){
        line.max = ifelse(labels.tilted, 2, 1) + 1
    }

    if(isLog){
        if(!niceLabels){
            axis(1, at = (info[-1] + info[-length(info)])/ 2, lwd = 0, lwd.ticks = 1, labels = round(exp(as.numeric(names(x))))[-1])
        } else {
            axis1Opts = list(at = (info[-1] + info[-length(info)])/ 2, labels = round(exp(as.numeric(names(x))))[-1], trunc = trunc, trunc.method = trunc.method, line.max = line.max)
            do.call(funLabels, axis1Opts)
        }
    } else {
        if(!niceLabels){
            axis(1, at = info, lwd = 0, labels = names(x))
        } else {
            axis1Opts = list(at = info, labels = names(x), trunc = trunc, trunc.method = trunc.method, line.max = line.max)
            # axis1Opts$at = info
            # axis1Opts$labels = names(x)
            do.call(funLabels, axis1Opts)
        }
    }

    # The stuff to be displayed on top of the bars
    if(top == "nb"){
        text(info, x_share, labels = addCommas(x), pos = 3, cex = cex.text)
    } else if(top == "frac"){
        text(info, x_share, labels = addCommas(x_share), pos = 3, cex = cex.text)
    }

    if(doTrim){
        legend("topright", legend = paste0(cases_left, " cases remaining (", round(100*share_left,1), "%)"), bty = "n")
    }


}

plot_line = function(x, y, addFit = FALSE, add = FALSE, smoothing_window = 0, ...){

    if(missing(y)){
        y_miss = TRUE
    } else {
        y_miss = FALSE
    }

    x_name = deparse(substitute(x))
    y_name = deparse(substitute(y))

    # Rmake the call
    dots <- list(...)
    if(is.null(dots$xlab)){
        if(y_miss){
            dots$xlab = ""
        } else {
            dots$xlab = x_name
        }
    }
    if(is.null(dots$ylab)){
        if(y_miss){
            dots$ylab = x_name
        } else {
            dots$ylab = y_name
        }
    }


    # If y-axis is missing
    if(y_miss){
        # we put y in the proper axis
        y = as.numeric(x)

        # we add the info of the x-axis
        if(!is.null(names(x)) && all(!grepl("[^[:digit:]]", names(x)))){
            x = as.numeric(names(x))
        } else {
            x = 1:length(y)
        }
    }

    # Re-ordering
    myOrder = order(x)
    x = x[myOrder]
    y = y[myOrder]

    # Smoothing
    if(smoothing_window>0){
        # we plot only numeric values
        val = val_origin = as.numeric(y)
        n = length(x)

        for(w in 1:smoothing_window){
            val = val + c(rep(NA, w), val_origin[1:(n-w)]) + c(val_origin[(w+1):n], rep(NA, w))
        }
        val = val / (2*smoothing_window + 1)

        y = val
    }

    dots$x = x
    dots$y = y
    listDefault(dots, "lwd", 2)
    listDefault(dots, "pch", 20)

    # Plot
    if(!add){
        dots$type = "n"
        do.call("plot", dots)
        grid(col = "darkgray")
    }

    dots$type = "o"
    do.call("lines", dots)

    if(addFit){
        res = lm(y~x)
        abline(res, lty=3)
    }

    if(smoothing_window>0 && !add){
        where = tell_me_where(x, y)
        legend(where, paste0(smoothing_window, "-period", ifelse(smoothing_window>1, "s", ""), " Moving Average"), bty = "n")
    }

}





box_single = function(x, y_min, q1, med, q3, y_max, xRight, width, inCol = NA, outCol = "black", lwd = 2, lwd.med = lwd + 2, density = -1){
    # Optional: xRight/width: we need one of the two!
    # either it is x, xRight, meaning that the rect x-dimension will be xLeft and xRight
    # either it is x, width, meaning that the rect x-dimension will be x-width/2 and x+width/2
    # outlier: do we show outliers??
    # showMean: do we show the mean?

    n = length(x)

    if(any(sapply(list(y_min, q1, med, q3, y_max), length) != n)){
        stop("One of the quantiles is not of the same length as 'x'.")
    }

    if(!missing(xRight) && !is.null(xRight)){
        if(length(xRight) != n){
            stop("The length of 'xRight' must be the same as x.")
        }
        xLeft = x
        xCenter = (xLeft + xRight)/2
        width = xRight - xLeft
    } else if(!missing(width) && !is.null(width)){
        if(!length(width) %in% c(1, n)){
            stop("The argument 'width' must be of length 1, or as the same length as 'x'.")
        }
        xCenter = x
        xLeft = x - width/2
        xRight = x + width/2
        if(length(width) == 1) width = rep(width, n)
    } else {
        stop("You must provide one of the arguments 'xRight' or 'width'.")
    }


    all_inCol = inCol[1 + (0:(n-1))%%length(inCol)]
    all_outCol = outCol[1 + (0:(n-1))%%length(outCol)]
    all_densities = density[1 + (0:(n-1))%%length(density)]

    for(i in 1:n){

        outCol = all_outCol[i]

        # The rectangle
        # rect(xleft = xLeft[i], ybottom = q1[i], xright = xRight[i], ytop = q3[i], col = inCol, lwd = lwd, border = outCol)
        rect(xleft = xLeft[i], ybottom = q1[i], xright = xRight[i], ytop = q3[i], col = all_inCol[i], lwd = lwd, border = all_outCol[i], density = all_densities[i])

        # The median
        segments(x0 = xLeft[i], y0 = med[i], x1 = xRight[i], y1 = med[i], lwd = lwd.med, col = outCol)

        # finding outliers
        span = 1.5*(q3[i] - q1[i])
        y_bottom = max(q1[i] - span, y_min[i])
        y_upper = min(q3[i] + span, y_max[i])

        #
        # moustache (M)
        #

        xLeftM = xLeft[i] + width[i]/4
        xRightM = xRight[i] - width[i]/4

        # top
        segments(x0 = xCenter[i], y0 = q3[i], x1 = xCenter[i], y1 = y_upper, lwd=lwd, col = outCol)
        segments(x0 = xLeftM, y0 = y_upper, x1 = xRightM, y1 = y_upper, lwd=lwd, col = outCol)

        # bottom
        segments(x0 = xCenter[i], y0 = q1[i], x1 = xCenter[i], y1 = y_bottom, lwd=lwd, col = outCol)
        segments(x0 = xLeftM, y0 = y_bottom, x1 = xRightM, y1 = y_bottom, lwd=lwd, col = outCol)
    }

}

abplot <- function(x, y, where="default", signifCode = c("***" = 0.001, "**" = 0.05, "*" = 0.10), log = FALSE, legend=TRUE, ...){
    #plot a graph with the linear fit
    #where: where to place the legend

    # we take care of xlabs
    dots <- list(...)
    if(is.null(dots$xlab)){
        dots$xlab = deparse(substitute(x))
        if(log) dots$xlab = paste0("ln(", dots$xlab, ")")
    }

    if(is.null(dots$ylab)){
        dots$ylab = deparse(substitute(y))
        if(log) dots$ylab = paste0("ln(", dots$ylab, ")")
    }


    if(log){

        qui = which(x<=0 | y<=0)
        if(length(qui)>0){
            warning(length(qui), " observations were omitted because of the log transformation.", call. = FALSE, immediate. = TRUE)
            x = x[-qui]
            y = y[-qui]
        }

        x = base::log(x)
        y = base::log(y)
    }

    dots$x = x
    dots$y = y

    do.call(plot, dots)

    r = lm(y~x)
    abline(r)
    a = r$coefficients[1]
    b = r$coefficients[2]
    s = summary(r)
    r2 = s$r.squared
    pval = s$coefficients[2,4]
    star = as.character(cut(pval, breaks = c(-1, signifCode, 100), labels = c(names(signifCode), "n.s.")))


    # setting the default of 'where'
    if(where == "default"){
        where = tell_me_where(x, y)
    }

    # the text to be displayed
    if(log){
        if(b<0){
            myEq = substitute(widehat(ln(Y))==a-b%*%ln(X)^s~~~~phantom(0), list(a=signif(a, 2), b=-(signif(b, 2)), s=star))
        } else {
            myEq = substitute(widehat(ln(Y))==a+b%*%ln(X)^s~~~~phantom(0), list(a=signif(a, 2), b=signif(b, 2), s=star))
        }
    } else {
        if(b<0){
            myEq = substitute(hat(Y)==a-b%*%X^s~~~~phantom(0), list(a=signif(a, 2), b=-(signif(b, 2)), s=star))
        } else {
            myEq = substitute(hat(Y)==a+b%*%X^s~~~~phantom(0), list(a=signif(a, 2), b=signif(b, 2), s=star))
        }
    }

    if(legend) legend(where,legend = c(myEq, substitute(R^2 == r2, list(r2=signif(r2, 2))), expression()),  cex=.8, bty="n")
}

find_margins_left = function(ylab, y_labels, ylab.resize){
    # ylab = "This is a very long message that will need to be cut because it is verbose and way too long"
    # LATER: add cex as argument

    # First: we resize
    if(ylab.resize){
        width_ok_in = par("pin")[2]
        current_width_in = strwidth(ylab, units = "in")
        if(current_width_in > width_ok_in){
            new_msg = list()
            unit_w = strwidth(" ", units = "in")
            msg_split = strsplit(ylab, " ")[[1]]

            n_return = 0

            while(n_return < 2){
                n_return = n_return + 1
                all_w = strwidth(msg_split, units = "in")
                cum_w = cumsum(all_w + unit_w) - unit_w
                qui = max(which.max(cum_w > width_ok_in) - 1, 1)
                new_msg[[length(new_msg) + 1]] = paste(msg_split[1:qui], collapse = " ")
                msg_split = msg_split[-(1:qui)]

                if(sum(strwidth(msg_split, units = "in") + unit_w) - unit_w < width_ok_in || n_return == 2){
                    new_msg[[length(new_msg) + 1]] = paste(msg_split, collapse = " ")
                    break
                }
            }

            ylab = paste(unlist(new_msg), collapse = "\n")

        }
    }

    line_height = par("mai")[1] / par("mar")[1]

    lab.width_in = max(strwidth(y_labels, units = "in"))

    ylab.line = 2 + lab.width_in / line_height

    nlines = lab.width_in / line_height + 2
    if(ylab != ""){
        nlines = nlines + ceiling(strheight(ylab, units = "in") / line_height)
    }

    total_width = nlines * line_height

    list(ylab = ylab, ylab.line = ylab.line, total_width = total_width)
}

find_margins_bottom = function(xlab, sub, data_freq, log, isNum, numLabel, numAxis, nbins, DO_SPLIT, ADD_OTHER, ADD_OTHER_LEFT, sorted, labels.tilted, delayLabelsTilted, checkForTilting, checkNotTilted, noSub, binned_data, line.max, trunc, trunc.method, cex.axis, labels.angle, at_5, xlim){
    # This function finds the size of the margin needed to display all the x-axis labels + xlab + sub
    # This is highly complex because the decision on how to show the x-axis labels
    # depend on many things in plot_distr.
    #
    # So far I didn't find a good solution to handle this
    #
    # I duplicated the code from plot_distr, then I gather the information painstackingly
    # THIS IS CRAPPY!!!! very hard to maintain, but I didn't find a better solution for now
    #
    # SOLUTION 1 (that does not work well):
    #  - do all the processsing here. Return a list containing
    #   * xaxis_label, xaxis_tilted, axis
    #   * then create the calls to these functions in plot_distr
    # - PROBLEM: I can't really do that because I might need svl calls to these functions...
    # eg sometimes I add ticks manuall, sometimes I add labels sequentially in a loop, etc...
    #

    xright = xleft = x_nb = x = isOther = x_num = mid_point = NULL

    at_info = data_freq[, list(mid_point = (max(xright) + min(xleft)) / 2), by = list(x_nb, x)]
    myat = at_info$mid_point

    LINE_MIN_TILTED = 0
    nlines = 0

    if(log){

        if(DO_SPLIT){

            data_freq_valid = data_freq[isOther == FALSE, ]
            data_freq_valid[, x_num := as.numeric(x)]
            x_all = data_freq_valid$x_num
            exp_value = ceiling(exp(x_all))
            exp_value[x_all == -1] = 0

            if(sorted){

                if(delayLabelsTilted){
                    # better display
                    labels.tilted = TRUE
                }

                myat = data_freq_valid[, (xleft+xright)/2]

                exp_value_right = ceiling(exp(x_all + 1))

                # Formatting
                exp_value_format = formatAxisValue(exp_value)
                exp_value_right_format = formatAxisValue(exp_value_right)
                label_displayed = paste0("[", exp_value_format, "; ", exp_value_right_format, ")")

                # finding the location
                if(labels.tilted){
                    info_tilt = xaxis_biased(at = myat, line.max = line.max, labels = label_displayed, only.params = TRUE)
                    nlines = info_tilt$height_line + LINE_MIN_TILTED
                } else {
                    lab.info = xaxis_labels(at = myat, labels = label_displayed, only.params = TRUE, xlim = xlim)
                    nlines = nlines + lab.info$height_line
                }

            } else {
                # we draw the axes with nice display

                moreLine = any(data_freq$isOther) * .25

                # Displaying the ticks "all at once" (including the last one)
                mysep = (data_freq$xleft[2] - data_freq$xright[1]) / 2

                exp_value_right = ceiling(exp(x_all + 1))

                # on the right
                myat = data_freq_valid$xright + mysep

                # We add the first tick on the left
                data_first = data_freq_valid[x_nb == 1]
                myat = c(data_first$xleft - mysep, myat)
                # exp_value = c(ceiling(exp(data_first$x_num - 1)), exp_value)
                first_val = ceiling(exp(data_first$x_num - 1))
                first_val[data_first$x_num == -1] = 0
                exp_value = c(first_val, exp_value_right)

                exp_value_format = formatAxisValue(exp_value)

                # tick location

                # 1) the ticks
                ## axis(1, at = myat, labels = NA, lwd.ticks = 1, lwd = 0, line = moreLine)

                # 2) The labels
                # Tilted labels not implemented for this axis
                if(delayLabelsTilted){

                    axis_info = xaxis_labels(at = myat, labels = exp_value_format, trunc = trunc, trunc.method = trunc.method, only.params = TRUE, xlim = xlim)
                    if(length(unique(axis_info$line)) == 1){
                        labels.tilted = FALSE
                    } else {
                        labels.tilted = TRUE
                    }

                }

                if(labels.tilted){
                    lab.info = xaxis_biased(at = myat, labels = exp_value_format, yadj = 2, angle = 25, only.params = TRUE)
                    nlines = nlines + lab.info$height_line + 1.5
                } else {
                    ## axis(1, at = myat, labels = exp_value_format, line = moreLine, lwd = 0)
                    nlines = nlines + 2 + moreLine
                }

            }

        } else {
            #
            # formatting of the values
            #

            x_unik = at_info[x_nb %in% 1:nbins, x]
            x_cases = length(x_unik)
            myat = at_info[x_nb %in% 1:nbins, mid_point]
            exp_value = ceiling(exp(x_unik))
            exp_value[x_unik == -1] = 0

            if(is.unsorted(x_unik) || x_cases == 1 || any(diff(x_unik) != 1)){
                exp_value_right = ceiling(exp(x_unik + 1))

                # Formatting
                exp_value_format = substr(exp_value, 1, 7)
                exp_value_right_format = substr(exp_value_right, 1, 7)

                # finding the location
                location = xaxis_labels(at = myat, labels = paste0("[", exp_value_format, "; ", exp_value_right_format, "["), only.params = TRUE, xlim = xlim)
                nlines = nlines + 2 + 1 + location$height_line
                # 2: axis
                # 1: see below 1 + location$line[i]

                # drawing
                # for(i in 1:length(x_unik)){
                #
                #     value = substitute(group("[",list(x1, x2),")"), list(x1 = formatAxisValue(exp_value[i]), x2 = formatAxisValue(exp_value_right[i])))
                #
                #     ## axis(1, at = myat[i], lwd = 0, labels = value, cex = location$cex, line = 1 + location$line[i])
                # }


            } else {
                # we draw the axes with nice display

                moreLine = (ADD_OTHER || ADD_OTHER_LEFT) * .25

                # Displaying the ticks "all at once" (including the last one)
                val = c(exp_value, ceiling(exp(tail(x_unik, 1) + 1)))
                exp_value_format = formatAxisValue(val)


                # tick location
                # loc = (1:length(val)-1)*(moderator_cases+sep) - sep/2
                ## axis(1, at = loc, labels = exp_value_format, line = moreLine, lwd.ticks = 1, lwd = 0)
                nlines = nlines + 2 + moreLine

            }

        }

    } else if(numLabel){
        # moderator > 1 + split + numeric axis

        current_lim = get_x_lim()

        nlines = nlines + 2

        # We add the bin information
        if(noSub){
            sub = "Bin size"
        }

    } else if(numAxis){

        # We add the bin information
        if(noSub){
            sub = "Bin Size"
        }

        nlines = nlines + 2

    } else if(DO_SPLIT){
        # we need to display all xs

        # We add the bin information => specific case: max first + numeric data
        if(binned_data && noSub){
            sub = "Bin size"
        }

        data_freq[, mid_point := (xleft + xright) / 2]
        myLabels = data_freq$x
        myAt = data_freq$mid_point


        if(checkNotTilted){
            # If very short labels => we don't tilt them // allows to reintroduce xlab

            # axis_info = xaxis_labels(at = myAt, labels = myLabels, only.params = TRUE)
            # # if we reduce the labels => we tilt them
            # labels.tilted = axis_info$cex < 1 || any(axis_info$line != -1)
            axis_info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, only.params = TRUE, xlim = xlim)
            if(length(unique(axis_info$line)) == 1){
                labels.tilted = FALSE
            } else {
                labels.tilted = TRUE
            }
        }

        if(labels.tilted){
            lab.info = xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, line.max = line.max, only.params = TRUE)
            nlines = nlines + lab.info$height_line + LINE_MIN_TILTED
        } else {
            lab.info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, only.params = TRUE, xlim = xlim)
            nlines = nlines + lab.info$height_line
        }


    } else if(isNum){
        # we can have the "other" column both left and right

        x_unik = at_info[x_nb %in% 1:nbins, x]
        myLabels = x_unik
        myAt = at_info[x_nb %in% 1:nbins, mid_point]

        info_axis = NULL
        if(labels.tilted == FALSE && mean(diff(x_unik)) == 1){
            # This is a "normal" axis
            # everything number follows, this is fine

            ## axis(1, myAt, labels = myLabels)
            nlines = nlines + 2
        } else {
            if(checkForTilting){
                # If normal axis does not fit => tilt
                axis_info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, only.params = TRUE, xlim = xlim)
                if(axis_info$failed){
                    labels.tilted = TRUE
                } else {
                    labels.tilted = FALSE
                }
            }

            if(labels.tilted){
                lab.info = xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, line.max = line.max, only.params = TRUE)
                nlines = nlines + lab.info$height_line + LINE_MIN_TILTED
            } else {
                lab.info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, line.max = line.max, only.params = TRUE, xlim = xlim)
                nlines = nlines + lab.info$height_line
            }
        }

    } else {

        if(ADD_OTHER){
            nbins = nbins + 1
            at_info$x[nbins] = "Other"
        }

        x_unik = at_info[x_nb %in% 1:nbins, x]
        myLabels = x_unik
        myAt = at_info[x_nb %in% 1:nbins, mid_point]

        if(checkForTilting){
            # If normal axis does not fit => tilt
            axis_info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, only.params = TRUE, xlim = xlim)

            if(axis_info$failed){
                labels.tilted = TRUE
            } else {
                labels.tilted = FALSE
            }
        }

        # We also add ticks every 5/10 bins to help counting
        if(missnull(at_5)){
            at_5 = ifelse(max(at_info$x_nb) > 10, TRUE, FALSE)
            if(at_5) {
                at_5 = ifelse(labels.tilted, "line", "roman")
            }
        } else {
            at_5 = at_5[1]
        }

        if(labels.tilted){
            lmin = 0.45 * (at_5 == "roman")
            lab.info = xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, line.max = line.max, line.min = lmin, only.params = TRUE)
            nlines = nlines + lab.info$height_line + LINE_MIN_TILTED + lmin
        } else {
            lmin = 0.25 * (at_5 == "roman")
            lab.info = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, line.min = lmin, only.params = TRUE, xlim = xlim)
            nlines = nlines + lab.info$height_line + lmin
        }

    }

    line_height = par("mai")[1] / par("mar")[1]

    xlab.line = nlines + 0.5 - 0.5*labels.tilted

    if(xlab != ""){
        xlab.line = xlab.line + ceiling(strheight(xlab, units = "in") / line_height) - 1
    }

    sub.line = xlab.line + 1

    if(sub != ""){
        nlines = sub.line + 1
    } else if(xlab != ""){
        nlines = sub.line
    }

    total_height = nlines * line_height

    list(xlab.line = xlab.line, sub.line = sub.line, total_height = total_height)
}


####
#### Utilities ####
####


set_defaults = function(opts_name){

    opts = getOption(opts_name)
    if(is.null(opts) || length(opts) == 0){
        return(NULL)
    }

    sysOrigin = sys.parent()
    mc = match.call(definition = sys.function(sysOrigin), call = sys.call(sysOrigin), expand.dots = FALSE)
    args_in = names(mc)

    for(v in names(opts)){
        if(!v %in% args_in){
            assign(v, opts[[v]], parent.frame())
        }
    }


}


char2num = function(x, addItem = FALSE){
    # we transform the data to numeric => faster analysis

    # special case
    qui = which(x == "")
    if(length(qui) > 0){
        x[qui] = "xxEMPTYxx"
    }

    x_unik = unique(x)
    dict = 1:length(x_unik)
    names(dict) = x_unik
    x_num = dict[x]

    names(x_num) = NULL

    if(addItem){
        res = list(x = x_num, items = x_unik)
        return(res)
    } else {
        return(x_num)
    }

}

quickUnclassFactor = function(x, addItem = FALSE){
    # does as unclass(as.factor(x))
    # but waaaaay quicker

    if(!is.numeric(x)){
        # level and unclass are slower than applying char2num (about 2 times)
        x = as.character(x)
    }

    if(is.character(x)){
        res = char2num(x, addItem)
        return(res)
    }

    myOrder = order(x)
    x_sorted = x[myOrder]
    x_quf_sorted = cpp_unclassFactor(x_sorted)
    x_quf = x_quf_sorted[order(myOrder)]

    if(addItem){
        res = list(x = x_quf, items = cpp_unik(x_sorted, tail(x_quf_sorted, 1)))
        return(res)
    } else {
        return(x_quf)
    }
}

getNames = function(x, dict = getOption("fplot_dict")){

    if(is.null(dict)){
        return(x)
    }

    dict_names = names(dict)
    if(is.null(dict_names)){
        stop("The dictionnary dict must be a named vector. Currently it has no names.")
    }

    x_clean = gsub(" +", "", x)
    res = x
    who_in = x_clean %in% names(dict)
    if(any(who_in)){
        res[who_in] = dict[x_clean[who_in]]
    }

    res
}

dict_apply = function(x, dict){
    # If here: dict is either a logical scalar, either a named character vector

    if(is.logical(dict)){
        if(dict == FALSE) return(x)

        dict = getFplot_dict()
    } else {
        dict_origin = getFplot_dict()

        if(!is.null(dict_origin)){
            if(!is.null(dict)){
                dict_origin[names(dict)] = as.vector(dict)
            }

            dict = dict_origin
        }
    }

    if(is.null(dict)){
        return(x)
    }

    dict_names = gsub(" ", "", names(dict), fixed = TRUE)
    x_clean = gsub(" ", "", x, fixed = TRUE)
    res = x
    who_in = x_clean %in% dict_names
    if(any(who_in)){
        res[who_in] = dict[x_clean[who_in]]
    }

    res

}

missnull = function(x){
    if(missing(x) || is.null(x)){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

listDefault = function(x, variable, value){
    # This function puts 'value' into the element 'variable' of list 'x'
    # IF it does not already exists in 'x'

    x_name = deparse(substitute(x))

    if(is.null(x[[variable]])){
        x[[variable]] = value

        assign(x_name, x, envir = parent.frame(n = 1))
    }

}

addCommas = function(x){

    addCommas_single = function(x){
        # Cette fonction ajoute des virgules pour plus de
        # visibilite pour les (tres longues) valeurs de vraisemblance

        # This is an internal function => the main is addCommas

        if(!is.finite(x)) return(as.character(x))

        s = sign(x)
        x = abs(x)
        decimal = x - floor(x)
        if (decimal > 0){
            dec_string = substr(decimal, 2, 4)
        } else {
            dec_string = ""
        }

        entier = sprintf("%.0f", floor(x))
        quoi = rev(strsplit(entier, "")[[1]])
        n = length(quoi)
        sol = c()
        for (i in 1:n) {
            sol = c(sol, quoi[i])
            if (i%%3 == 0 && i != n) sol = c(sol, ",")
        }
        res = paste0(ifelse(s == -1, "-", ""), paste0(rev(sol), collapse = ""),
                     dec_string)
        res
    }

    sapply(x, addCommas_single)
}


table_collapse = function(x, sorted = TRUE){
    # DT VARS USED
    id = NULL

    # Faster than table thx to data.table
    info = data.table(id = x)

    grouped = info[, list(n = .N), by = id]

    if(sorted){
        grouped = grouped[order(id)]
    }

    res = grouped$n
    names(res) = grouped$id

    res
}


extract_pipe = function(fml){
    # We extract the elements after the pipe

    FML = Formula::Formula(fml)
    n_fml = length(FML)
    n_lhs = n_fml[1]
    n_rhs = n_fml[2]

    if(n_lhs == 0){
        # If no LHS, fine, but we recreate a two sided formula
        FML = Formula::Formula(update(FML, 1~.))
        n_lhs = 1
    }

    if(n_rhs == 1){
        fml_new = formula(FML, lhs = n_lhs, rhs = 1)
        lhs_fml = ~x1
        lhs_fml[[2]] = formula(FML, lhs = n_lhs, rhs = 0)[[2]]
        pipe = pipe_fml = NULL
    } else if(n_rhs == 2){
        fml_new = formula(FML, lhs = 1, rhs = 1)
        lhs_fml = pipe_fml = ~x1
        lhs_fml[[2]] = formula(FML, lhs = n_lhs, rhs = 0)[[2]]
        pipe = as.expression(formula(FML, lhs = 0, rhs = 2)[[2]])
        pipe_fml[[2]] = formula(FML, lhs = 0, rhs = 2)[[2]]
    } else {
        stop_up("Argument 'fml' must be at *most* a two part formula (currently it is ", n_rhs, " parts).")
    }

    list(fml = fml_new, lhs_fml = lhs_fml, pipe = pipe, pipe_fml = pipe_fml)
}

sunique = function(x){

    if(!checkVector(x) || is.list(x)){
        stop("x must be a vector!")
    }

    if(is.factor(x)){
        x = as.character(x)
    }

    # we use data.table (faster for characters)

    # DT VARS USED
    id = NULL

    data = data.table(id = x)
    res <- unique(data)
    res <- res[order(id)]
    res$id
}

to01 = function(x, minmax){
    check_arg(x, minmax, "numeric vector")

    if(all(is.na(x))) return(x)

    if(!missing(minmax) && !is.null(minmax)){
        MIN = minmax[1]
        MAX = minmax[2]
    } else {
        MIN = min(x, na.rm = TRUE)
        MAX = max(x, na.rm = TRUE)
    }

    if(anyNA(x)){
        res = rep(NA, length(x))
        qui_ok = !is.na(x)
        x_ok = x[qui_ok]
        res[qui_ok] = (x_ok - MIN) / (MAX - MIN)
    } else {
        res = (x - MIN) / (MAX - MIN)
    }
    res
}

dict2number = function(dict, x){
    # this function takes in a vector of (unique) identifiers
    # and a vector that should put x into numbers
    # it returns the numbers associated to the identifier

    # DT VARS USED
    id = id_obs = NULL

    # check
    if(length(dict) != length(unique(dict))) stop("The argument 'dict' should be a vector of UNIQUE identifiers.")

    if(!identical(class(dict), class(x))){
        warning("'dict' and 'x' are of different types (", 
                class(dict)[1], " vs ", class(x)[1], "): they are converted to character.", 
                call. = FALSE)
        # we put the two in characters
        dict = as.character(dict)
        x = as.character(x)
    }

    n = length(dict)
    nx = length(x)
    base_dict = data.table(id = dict, id_num = 1:n)
    base_main = data.table(id = x, id_obs = 1:nx)
    setkey(base_dict, id)
    setkey(base_main, id)
    res = merge(base_main, base_dict, all.x = TRUE, by = "id")
    res = res[order(id_obs)]

    return(res$id_num)
}


rbindDS <- function(x, y){
    # This function merges two data.frames
    # it uses their names for the merging, therefore, they
    # don't need to have the same number of columns
    # The first DF can be empty

    if(!is.null(x)){
        if(!inherits(x, "data.frame")){
            stop("x must be a data.frame/data.table.")
        }
    }

    if(length(y) == 0) return(x)

    if(!inherits(y, "data.frame") && !(checkVector(y) & !is.null(names(y)))){
        stop("If argument 'y' is a vector, it must be named!")
    }

    if(checkVector(y)) y = as.data.frame(t(y))

    if(is.null(y) || nrow(y) == 0) return(x)
    if(is.null(x) || nrow(x) == 0) return(y)

    names_x = names(x)
    names_y = names(y)

    allNames = unique(c(names_x, names_y))

    new_names_y = setdiff(names_x, names_y)
    new_names_x = setdiff(names_y, names_x)

    for(var in new_names_x) x[[var]] = NA
    for(var in new_names_y) y[[var]] = NA

    if(is.data.table(x)){

        if(nrow(y) >= 1 && !is.data.table(y)){
            warning("x is a data.table while y is a data.frame: result is coerced to data.table.")
            y_copy = as.data.table(y)
            res = rbindlist(list(x[, allNames, with = FALSE], y_copy[, allNames, with = FALSE]))
        } else {
            res = rbindlist(list(x[, allNames, with = FALSE], y[, allNames, with = FALSE]))
        }
    } else {
        if(is.data.table(y)){
            warning("x is a data.frame while y is a data.table: result is coerced to data.table.")
            x_copy = as.data.table(x)
            res = rbindlist(list(x_copy[, allNames, with = FALSE], y[, allNames, with = FALSE]))
        } else {
            res = rbind(x[allNames], y[allNames])
        }
    }

    return(res)
}

checkVector = function(x){
    # it seems that when you subselect in data.table
    # sometimes it does not yield a vector
    # so i cannot use is.vecyor to check the consistency

    if(is.vector(x)){
        return(TRUE)
    } else if(length(class(x)) == 1){
        if(class(x) %in% c("integer", "numeric", "character", "factor", "Date") && is.null(dim(x))){
            return(TRUE)
        }
    }
    return(FALSE)
}

mysignif = function(x, d=2, r=1){

    # The core function
    mysignif_single = function(x, d, r){
        if(is.na(x)) return(NA)

        if(abs(x)>=10**(d-1)) return(round(x, r))
        else return(signif(x, d))
    }

    # the return
    sapply(x, mysignif_single, d=d, r=r)
}

numberFormat = function(x, d=2, r=1){
    numb_char = as.character(x)
    quiHigh = (abs(x) >= 1e4 & !is.na(x))
    if(sum(quiHigh) > 0){
        numb_char[quiHigh] = addCommas(mysignif(x[quiHigh], d=d, r=r))
    }

    if(sum(!quiHigh) > 0){
        numb_char[!quiHigh] = as.character(mysignif(x[!quiHigh], d=d, r=r))
    }

    numb_char
}

clean_name = function(x){
    if(grepl("\\$[[:alnum:]_.]+$", x)){
        return(gsub(".+\\$", "", x))
    } else if(grepl("\\[\\[(('[^']+')|(\"[^\"]+\"))\\]\\]$", x)){
        return(gsub("(^.+\\[\\[(\"|'))|((\"|')\\]\\]$)", "", x))
    } else {
        return(x)
    }
}

# Avoids the problem of multiple lines deparse
deparse_long = function(x){
    dep_x = deparse(x)
    if(length(dep_x) == 1){
        return(dep_x)
    } else {
        return(paste(gsub("^ +", "", dep_x), collapse = ""))
    }
}


isVector = function(x){

    if(is.atomic(x) && is.null(dim(x))){
        return(TRUE)
    }

    return(FALSE)
}

####
#### DEPRECATED ####
####


plot_bar = function(fml, data, agg, fun = mean, dict = getFplot_dict(), order=FALSE, nbins=50, show0=TRUE, cex.text=0.7, isDistribution = FALSE, yaxis.show = TRUE, labels.tilted, trunc = 20, trunc.method = "auto", line.max, hgrid = TRUE, top = "nb", showOther = TRUE, inCol = "#386CB0", border = "white", xlab, ylab, ...){
    # this function formats a bit the data and sends it to myBarplot

    # Old params
    isLog = FALSE

    fml_in = fml

    # Controls
    check_arg("logical scalar", order, show0, isDistribution, labels.tilted, hgrid, showOther)

    check_arg(nbins, trunc, "integer scalar GE{1}")
    check_arg(trunc.method, "character scalar")

    #
    # Extracting the information
    #

    mc = match.call()
    if("fun" %in% names(mc)){
        fun_name = paste0(" (", deparse(mc$fun), ")")
    } else {
        fun_name = " (Average)"
    }

    doAgg = TRUE
    if(inherits(fml_in, "formula")){
        # Control of the formula

        if(missing(data) || !is.data.frame(data)){
            postfix = ifelse(!is.data.frame(data), 
                             paste0(" Currently it is of class ", enumerate_items(class(data))), 
                             "")

            stop("If you provide a formula, a data.frame must be given in the argument 'data'.", postfix)
        }

        vars = all.vars(fml_in)
        if(any(!vars %in% names(data))){
            stop("The variable", enumerate_items(setdiff(vars, names(data)), "s.is")," not in the data set (", deparse(mc$data), ").")
        }

        # Creation of x and the condition
        if(!length(fml_in) == 3){
            stop("The formula must be of the type 'var ~ agg'.")
        }

        fml = extract_pipe(fml_in)$fml
        pipe = extract_pipe(fml_in)$pipe

        x = eval(fml[[2]], data)
        agg = eval(fml[[3]], data)

        if(length(agg) == 1){
            # No agg!
            doAgg = FALSE
            agg = 1:length(x)
            agg_name = ""
            fun_name = ""
        } else {
            agg_name = deparse(fml[[3]])
        }

        # other info
        x_name = paste0(deparse(fml[[2]]), fun_name)


    } else {

        x = fml_in

        if(missing(agg)){
            if(is.null(names(x))){
                agg = 1:length(x)
            } else {
                agg = names(x)
            }
            doAgg = FALSE
        } else if(length(x) != length(agg)){
            stop("The arguments 'x' and 'agg' must be of the same length.")
        }

        # other info
        x_name = ""
        agg_name = ""
    }

    # Naming
    x_name = dict_apply(x_name, dict)
    agg_name = dict_apply(agg_name, dict)

    # Dropping NAs
    quiNA_x = is.na(x)
    quiNA_agg = is.na(agg)
    quiNA = quiNA_x | quiNA_agg
    if(any(quiNA)){
        nb_na = c(sum(quiNA_x), sum(quiNA_agg))
        msg_na = paste0(c("x: ", "agg: "), nb_na)
        message("NOTE: ", sum(quiNA), " observations with NAs (", enumerate_items(msg_na[nb_na>0]), ")")
        x = x[!quiNA]
        agg = agg[!quiNA]
    }

    #
    # Aggregation
    #

    if(doAgg){
        AGG_FACTOR = FALSE
        if(is.factor(agg)){
            AGG_FACTOR = TRUE
            agg_names = levels(agg[, drop = TRUE])
            agg = unclass(agg[, drop = TRUE])
        }

        quoi = data.table(x=x, agg=agg)
        base_agg = quoi[, list(x = fun(x)), by = list(agg)]
        setorder(base_agg, agg)

        res = base_agg$x
        if(AGG_FACTOR){
            names(res) = agg_names
        } else {
            names(res) = base_agg$agg
        }

    } else {
        res = x
        names(res) = agg
    }


    #
    # Sending to myBarplot
    #

    # Some default values
    if(missnull(labels.tilted)){
        if(!is.numeric(agg)){
            size = sum(nchar(names(res)))
            if(size * strwidth("W", "in") > 1.5*par("pin")[1]){
                labels.tilted = TRUE
                agg_name = ""
            } else {
                labels.tilted = FALSE
            }
        } else {
            labels.tilted = FALSE
        }
    } else {
        if(labels.tilted){
            agg_name = ""
        }
    }

    if(missing(xlab)){
        xlab = agg_name
    }

    if(missing(ylab)){
        ylab = x_name
    }

    myBarplot(x = res, order=order, nbins=nbins, show0=show0, cex.text=cex.text, isLog=isLog, isDistribution = isDistribution, yaxis.show = yaxis.show, niceLabels = TRUE, labels.tilted=labels.tilted, trunc = trunc, trunc.method = trunc.method, line.max=line.max, hgrid = hgrid, top = top, showOther = showOther, inCol = inCol, outCol = border, xlab = xlab, ylab = ylab, ...)

    invisible(base_agg)
}






####
#### DOCUMENTATION DATA ####
####


#' Publication data sample
#'
#' This data reports the publications of U.S. institutions in the field of economics between 1985 and 1990.
#'
#' @usage
#' data(us_pub_econ)
#'
#' @format
#' \code{us_pub_econ} is a data table with 30,756 observations and 6 variables.
#'
#' * paper_id: Numeric identifier of the publication.
#' * year: Year of publication.
#' * institution: Institution of the authors of the publication.
#' * journal: Journal/conference name.
#' * jnl_top_25p: 0/1 variable of whether the journal belongs to the top 25% in terms of average cites.
#' * jnl_top_5p: 0/1 variable of whether the journal belongs to the top 5% in terms of average cites.
#'
#'
#' @source
#' The source is Microsoft Academic Graph (see reference).
#'
#' @references
#' Arnab Sinha, Zhihong Shen, Yang Song, Hao Ma, Darrin Eide, Bo-June (Paul) Hsu, and Kuansan Wang. 2015. An Overview of Microsoft Academic Service (MAS) and Applications. In Proceedings of the 24th International Conference on World Wide Web (WWW '15 Companion). ACM, New York, NY, USA, 243-246.
#'
#'
#'
"us_pub_econ"



####
#### peculiar dev stuff ####
####

is_r_check = function(){
	any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE))
}

renvir_get = function(key){
  # Get the values of envir variables
  # we also evaluate them

  value_raw = Sys.getenv(key)

  if(value_raw == ""){
      return(NULL)
  }

  # Any default value should be able to be evaluated "as such"
  value_clean = gsub("__%%;;", "\n", value_raw)
  value_clean = gsub("&quot;", '"', value_clean)
  value_clean = gsub("&apos;", "'", value_clean)

  value = eval(str2lang(value_clean))

  return(value)
}

is_package_root = function(){
  isTRUE(renvir_get("package_ROOT"))
}

fix_pkgwdown_path = function(){
    # https://github.com/r-lib/pkgdown/issues/1218
    # just because I use google drive... it seems pkgdown cannot convert to relative path...

    # This is to ensure it only works for me
    if(!is_package_root()) return(NULL)

    all_files = list.files("docs/articles/", full.names = TRUE, pattern = "html$")

    for(f in all_files){
        my_file = file(f, "r", encoding = "UTF-8")
        text = readLines(f)
        close(my_file)
        if(any(grepl("../../../", text, fixed = TRUE))){
            # We embed the images directly: safer
            
            message("pkgdown images updated: ", gsub(".+/", "", f))

            # A) we get the path
            # B) we transform to URI
            # C) we replace the line

            pat = "<img.+\\.\\./.+/fplot/.+/images/"
            qui = which(grepl(pat, text))
            for(i in qui){
                # ex: line = "<img src = \"../../../Google drive/fplot/fplot/vignettes/images/etable/etable_tex_2021-12-02_1.05477838.png\">"
                line = text[i]
                line_split = strsplit(line, "src *= *\"")[[1]]
                path = gsub("\".*", "", line_split[2])
                # ROOT is always fplot
                path = gsub(".+fplot/", "", path)
                path = gsub("^articles", "vignettes", path)

                URI = knitr::image_uri(path)

                rest = gsub("^[^\"]+\"", "", line_split[2])
                new_line = paste0(line_split[1], ' src = "', URI, '"', rest)

                text[i] = new_line
            }

            my_file = file(f, "w", encoding = "UTF-8")
            writeLines(text, f)
            close(my_file)
        }
    }

}
