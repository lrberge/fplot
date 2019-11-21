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
#' Sets/gets the default dictionary used to rename the axes/moderator variables in the functions of the package \code{fplot}. The dictionaries are used to relabel variables (usually towards a fancier, more explicit formatting) that can be useful not to explicitly use the arguments xlab/ylab when exporting graphs. By setting the dictionary with \code{setFplot_dict}, you can avoid providing the argument \code{dict} in \code{fplot} functions.
#'
#'
#' @param dict A named character vector. E.g. to change my variable named "us_md" and "state" to (resp.) "$ miilion" and "U.S. state", then use \code{dict = c(us_md="$ million", state = "U.S. state")}.
#'
#' @author
#' Laurent Berge
#'
#'
#' @examples
#'
#' \donttest{
#' library(fixest)
#' data(trade)
#' setFplot_dict(c(Origin = "Country of Origin", Euros = "Exportations"))
#' plot_distr(Origin~1|Euros, trade)
#' setFplot_dict()
#' }
#'
setFplot_dict = function(dict){

    if(missing(dict) || is.null(dict)){
        options("fplot_dict" = NULL)
        return(invisible())
    }

    #
    # Controls
    #

    if(!is.character(dict) || !checkVector(dict)){
        stop("Argument 'dict' must be a character vector.")
    }

    if(anyNA(dict)){
        stop("Argument 'dict' must be a character vector without NAs.")
    }

    # Formatting the names
    dict_names = names(dict)
    if(is.null(dict_names)){
        stop("Argument 'dict', the dictionary, must be a named vector. Currently it has no names.")
    }

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

####
#### Main Graph. Tools ####
####


truncate_string = function(x, trunc = 20, method = "auto"){

    control_variable(x, "(characterNumeric)Vector")
    control_variable(trunc, "singleIntegerGE3")

    method = control_variable(method, "singleCharacterMatch.arg", charVec = c("auto", "trimRight", "trimMid"))

    if(is.numeric(x)) x = as.character(x)

    n_all = nchar(x)

    if(method == "trimRight"){
        res = substr(x, 1, trunc)
        qui = nchar(res) == trunc & n_all > trunc
        res[qui] = gsub("..$", "\\.\\.", res[qui])

    } else if(method == "trimMid"){
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


xaxis_labels = function(at, labels, minLine = -1, max_line = 1, minCex = 0.8, add_ticks = FALSE, trunc = 20, trunc.method = "auto", onlyParams = FALSE, ...){
    # This function automates the placement of labels into the axis 1
    # It first put the cex to the appropritate level to insert the 1st
    # label into the frame, then
    # It uses the vertical placement to fit all the labels
    # if onlyParams => nothing is plotted

    # only 1 value => we do nothing
    if(length(at) == 1){
        if(onlyParams){
            res = list(cex = 1, line = minLine)
            return(res)
        }
        axis(1, at=at, labels = labels, tick = add_ticks)
        return(invisible(NULL))
    }

    # To send into a function: myat & lab
    myOrder = order(at)
    myLabels = labels[myOrder]
    myAt = at[myOrder]

    n = length(myAt)

    # # Truncation of the items
    # myLabels = substr(myLabels, 1, trunc)
    # # we replace the trucated element with dots to show that there is more
    # qui = nchar(myLabels) == trunc & nchar(labels[myOrder]) > trunc
    # myLabels[qui] = gsub("..$", "\\.\\.", myLabels[qui])
    if(!"call" %in% class(myLabels)){
        myLabels = truncate_string(myLabels, trunc, method = trunc.method)
    } else {
        myLabels = gsub(" *phantom\\([\\)]*\\) *", "", deparse(myLabels))
    }


    # We compute the space that is left to display the label
    # 1st into the plot
    largeur = diff(par("usr")[1:2])
    half_plotSpace_in = (myAt[1] - par("usr")[1]) / largeur * par("pin")[1]
    # 2nd using the margin
    total_half_space = half_plotSpace_in + min(par("mai")[c(2,4)])

    # If it is too large, we reduce it:
    myCex = 1
    while(myCex > minCex && strwidth(myLabels[1], units = "in", cex = myCex)/2 > total_half_space){
        myCex = myCex * 0.95
    }

    if(myCex < minCex) myCex = minCex

    # Now we order the vertical space where the labels will appear
    # line_step = max(strheight(myLabels, units = "in", cex = myCex)) / max(strheight(myLabels, units = "in"))
    # we use the fact that:
    # 1) smaller characters can be more stacked vertically
    # 2) a line height is almost equivalent to character height

    # all_lines = -1:max_line
    ok = FALSE
    failed = FALSE
    while(!ok){
        ok = TRUE # if !ok there's a problem, we need to reduce cex

        all_width = strwidth(myLabels, units = "in", cex = myCex)

        # there can be more lines than expected depending on cex
        line_step = max(strheight(myLabels, units = "in", cex = myCex)) / max(strheight(myLabels, units = "in"))
        all_lines = seq(minLine, max_line, by = line_step)

        myLine = current_Line = minLine
        for(i in 2:n){
            # for each element, we find out the line where to write it
            for(line_index in all_lines){
                # we get the closest index with that Line level
                if(line_index %in% myLine){
                    index = max(which(myLine == line_index))
                    # we look at the distance between the two elements
                    at_first = myAt[index]
                    at_second = myAt[i]
                    # the distance in inches between the two 'at' + the space of one letter
                    dist_in = (at_second - at_first) * par("pin")[1] / largeur - strwidth("O", units = "in", cex = 1)
                    # the half sizes of the two strings
                    half_sums_in = (all_width[index] + all_width[i]) / 2
                    if(half_sums_in > dist_in){
                        # we go to the next line_index
                    } else {
                        myLine = c(myLine, line_index)
                        break
                    }
                } else {
                    # this line item has not been used already
                    myLine = c(myLine, line_index)
                    break
                }

                if(line_index == max(all_lines)) {
                    # Means it does not fit => we need to reduce cex
                    if(myCex <= minCex){
                        # already at the minimum, we keep going then
                        myLine = c(myLine, line_index)
                        failed = TRUE
                    } else {
                        # we get out totally and reduce the cex
                        ok = FALSE
                        myCex = myCex * 0.95
                    }
                }
            }

            if(!ok){
                # This means we've been into a non solvable situation
                break
            }
        }
    }

    if(onlyParams){
        res = list(cex = myCex, line = myLine, failed = failed)
        return(res)
    }

    # We draw the ticks
    if(add_ticks){
        # 1) drawing them
        for(line in unique(myLine)){
            qui = which(myLine == line)
            axis(1, at = myAt[qui], labels = NA, tcl = -1-line, lwd = 0, lwd.ticks = 1)

        }

        # 2) "Cleaning" them
        n = length(myLabels)
        mid_width = strwidth(myLabels, units = "user") / 2

        # ceux qui 'debordent' a droite
        qui = which(mid_width[-n] > diff(myAt))
        if(length(qui) > 0){
            for(i in qui){
                # axis(1, at = myAt[i+1], labels = NA, lwd = 0, col = "white", line = myLine[i], lwd.ticks = 1.5)
                axis(1, at = myAt[i+1], labels = "|", lwd = 0, col.axis = "white", cex.axis = 1.5, line = myLine[i], lwd.ticks = 0)
            }
        }

        # ceux qui 'debordent' a gauche
        qui = which(mid_width[-1] > diff(myAt))
        if(length(qui) > 0){
            for(i in qui){
                # axis(1, at = myAt[i+1], labels = NA, lwd = 0, col = "white", line = myLine[i], lwd.ticks = 1.5)
                axis(1, at = myAt[i], labels = "|", lwd = 0, col.axis = "white", cex.axis = 1.5, line = myLine[i+1], lwd.ticks = 0)
            }
        }

    }

    # We draw the labels
    myLine = myLine + 0.2
    for(line in unique(myLine)){
        qui = which(myLine == line)
        # the labels
        axis(1, at = myAt[qui], labels = myLabels[qui], line = line, cex.axis = myCex, lwd = 0)

    }

    res = list(cex = myCex, line = myLine)
    return(res)

}


xaxis_biased = function(at, labels, angle, cex, max_line = 1, yadj = 0.5, trunc = 20, trunc.method = "auto", ...){

    control_variable(angle, "nullSingleNumeric")
    control_variable(cex, "nullSingleNumeric")


    dots = list(...)
    dots$x = at

    if(!"call" %in% class(labels)){
        labels_trunc = truncate_string(labels, trunc = trunc, method = trunc.method)
    } else {
        labels_trunc = gsub(" *phantom\\([\\)]*\\) *", "", deparse(labels))
    }

    dots$labels = labels_trunc

    # setting automatically the cex and angle
    DO_ALGO = FALSE
    if(missnull(angle)){
        angle2check = c(45, 35, 25, 20)
        DO_ALGO = TRUE
    } else {
        angle2check = angle
    }

    if(missnull(cex)){
        cex2check = c(1, 0.9, 0.8)
        DO_ALGO = TRUE
    } else {
        cex2check = cex
    }

    if(DO_ALGO){
        lab_max = labels_trunc[which.max(strwidth(labels_trunc))]
        n_angle = length(angle2check)
        w_all = rep(sapply(cex2check, function(x) strwidth(lab_max, "in", cex = x)), n_angle)*1.05
        angle_all = rep(angle2check, each = length(cex2check))
        longueur_cote = cos((90 - angle_all)/360*2*pi)*w_all

        line_height = par("mai")[1] / par("mar")[1]
        total_height = line_height * (max_line + 2 - yadj)

        qui = longueur_cote <= total_height
        if(any(qui)){
            i = which.max(qui)
            angle = angle_all[i]
            cex = rep(cex2check, n_angle)[i]
        } else {
            angle = tail(angle_all, 1)
            cex = tail(cex2check, 1)
        }
    }

    dots$cex = cex
    SH = strheight("WWW", units = "user")
    dots$y = par("usr")[3] - yadj*SH
    dots$srt = angle
    dots$xpd = TRUE
    dots$adj = 1

    do.call("text", dots)

    return(invisible(list(cex=cex, angle=angle)))
}


legendFit = function(where = "top", legend, minCex = 0.7, trunc, trunc.method = "auto", plot = TRUE, title = NULL, title_out = FALSE, ...){
    # units in inch to avoid the need of having a graph already plotted
    # (you cannot use par("usr) when there is no graph plotted)

    # the title
    control_variable(title, "nullSingle(CharacterNumeric)")
    control_variable(title_out, "singleLogical")
    ADD_TITLE = FALSE
    if(length(title) == 1 && nchar(title) > 0 && grepl("[^ ]", title)){
        ADD_TITLE = TRUE
    }
    # decalage vers le bas de la legende (ssi title_out = TRUE)
    do_adj = 0
    if(!ADD_TITLE){
        do_adj = -1
    } else if(!title_out){
        do_adj = 1
    }
    # do_adj = ADD_TITLE && !title_out

    # 1) Truncation of the items
    n = length(legend)

    AUTO_TRUNC = TRUE
    if(!missing(trunc)){
        AUTO_TRUNC = FALSE
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
        res$total_height = (4 + do_adj/2)*hauteur_caractere
    } else {
        res$total_height = (2.5 + do_adj/2)*hauteur_caractere
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
    control_variable(x, "numericVector", mustBeThere = TRUE)
    control_variable(percent, "singleNumeric")

    r_x = range(x)
    width = diff(r_x)
    r_x + c(-1, 1) * width/2 * percent/100
}


formatAxisValue = function(x, d = 2, r = 0, type = "abbrev"){
    # This function formats values to be displayed in the x-axis
    # It transforms them into easily readable format

    control_variable(d, "singleIntegerGE1")
    control_variable(r, "singleIntegerGE0")
    type = control_variable(type, "singleCharacterMatch.arg", charVec = c("abbrev", "plain", "signif", "equation"))


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

        x = log(x)
        y = log(y)
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

    control_variable(ymin, "singleNumeric")
    control_variable(ymax, "singleNumeric")

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

    control_variable(ymin, "singleNumeric")
    control_variable(ymax, "singleNumeric")

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


myHist = function(x, maxValue = +Inf, cex.text = 0.7, doubleTable = FALSE, toLog = FALSE, use_xaxis, inCol = "#386CB0", outCol = "white",  ...){
    # personalized histogram

    if(doubleTable){
        tx = ttable(x)
    } else {
        tx = round(x)
    }

    if(toLog){
        tx = floor(log(tx + 1e-6))
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

    ttx = ttable(tx)

    if(overMax & !toLog) names(ttx)[length(ttx)] = paste0(maxValue,"+")

    # New version
    dots = list(...)
    dots$axes = FALSE
    dots$axisnames = FALSE
    dots$col = 0
    dots$border = FALSE

    useAxis = FALSE
    if(!missing(use_xaxis)){
        if(!all(c("toLog", "all_names") %in% names(use_xaxis))) stop("You must give a myHist object in argument use_xaxis.")
        if(xor(toLog, use_xaxis$toLog)) stop("The 'log' status must be identical to the one in use_xaxis.")
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

    if(toLog){
        labels = round(exp(as.numeric(names(ttx_share))))[-1]

        axis(1, at = (info[-1] + info[-length(info)])/ 2, lwd = 0, lwd.ticks = 1, labels = labels)
    } else {
        labels = names(ttx_share)

        axis(1, at = info, lwd = 0, labels = labels)
        # xaxis_labels(at = info, labels = names(ttx), ...)
    }

    text(info, ttx_share, label = addCommas(ttx), pos = 3, cex = cex.text)

    invisible(list(toLog=toLog, all_names = names(ttx_share)))
}


myBarplot = function(x, order=FALSE, maxBins=10, show0=TRUE, cex.text=0.7, isLog=FALSE, isDistribution = TRUE, yaxis.show = TRUE, niceLabels = FALSE, labels.tilted=FALSE, axis1Opts = list(), hgrid = TRUE, onTop = "nb", showOther = TRUE, inCol = "#386CB0", outCol = "white", trunc=20, trunc.method = "auto", max_line, ...){
    # This function draws a nice barplot

    # We get whether the labels from x are numeric
    isNumericLabel = is.numeric(tryCatch(as.numeric(names(x)), warning = function(x) "problem"))

    # we transform x if necessary
    if(!show0) x = x[x>0] # we don't show values 0
    if(order) x = sort(x, decreasing = TRUE)

    doTrim = FALSE
    if(length(x) > maxBins) {

        if(!showOther){
            doTrim = TRUE
        } else {
            y = x[1:(maxBins-1)]

            # We change the name
            max_name = ifelse(isNumericLabel & !order, paste0(names(x)[maxBins], "+"), "other")
            allNames = names(y)

            # we recreate x
            z = c(y, sum(x[maxBins:length(x)]))
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
        cases_left = length(x) - maxBins
        sum_x = sum(x)
        x = x[1:maxBins]
        x_share = x_share[1:maxBins]
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
        if(onTop != "none"){
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

    if(missnull(max_line)){
        max_line = ifelse(labels.tilted, 2, 1)
    }

    if(isLog){
        if(!niceLabels){
            axis(1, at = (info[-1] + info[-length(info)])/ 2, lwd = 0, lwd.ticks = 1, labels = round(exp(as.numeric(names(x))))[-1])
        } else {
            axis1Opts = list(at = (info[-1] + info[-length(info)])/ 2, labels = round(exp(as.numeric(names(x))))[-1], trunc = trunc, trunc.method = trunc.method, max_line = max_line)
            do.call(funLabels, axis1Opts)
        }
    } else {
        if(!niceLabels){
            axis(1, at = info, lwd = 0, labels = names(x))
        } else {
            axis1Opts = list(at = info, labels = names(x), trunc = trunc, trunc.method = trunc.method, max_line = max_line)
            # axis1Opts$at = info
            # axis1Opts$labels = names(x)
            do.call(funLabels, axis1Opts)
        }
    }

    # The stuff to be displayed on top of the bars
    if(onTop == "nb"){
        text(info, x_share, label = addCommas(x), pos = 3, cex = cex.text)
    } else if(onTop == "frac"){
        text(info, x_share, label = addCommas(x_share), pos = 3, cex = cex.text)
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

        x = log(x)
        y = log(y)
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


####
#### Utilities ####
####


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


ttable = function(x, sorted = TRUE){
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
    n_rhs = n_fml[2]

    if(n_rhs == 1){
        fml_new = formula(FML, lhs = n_fml[1], rhs = 1)
        pipe = NULL
    } else if(n_rhs == 2){
        fml_new = formula(FML, lhs = 1, rhs = 1)
        pipe = as.expression(formula(FML, lhs = 0, rhs = 2)[[2]])
    } else {
        stop("fml must be at *most* a two part formula (currently it is ", n_rhs, " parts).")
    }

    list(fml=fml_new, pipe=pipe)
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
    control_variable(x, "numericVector", prefix = "to01: ")
    control_variable(x, "numericVector", prefix = "to01: ")

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

    if(class(dict) != class(x)){
        warning("'dict' and 'x' are of different types (", class(dict), " vs ", class(x), "): they are converted to character.", call. = FALSE)
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
        if(!"data.frame" %in% class(x)){
            stop("x must be a data.frame/data.table.")
        }
    }

    if(length(y) == 0) return(x)

    if(!"data.frame" %in% class(y) && !(checkVector(y) & !is.null(names(y)))){
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


####
#### DOCUMENTATION DATA ####
####


#' Publication data sample
#'
#' This data reports the publications of U.S. institutions in the field of biology between 1985 and 1990.
#'
#' @usage
#' data(us_pub_biology)
#'
#' @format
#' \code{us_pub_biology} is a data table with 150,066 observations and 6 variables.
#'
#' \itemize{
#' \item{paper_id: Numeric identifier of the publication.}
#' \item{year: Year of publication.}
#' \item{institution: Institution of the authors of the publication.}
#' \item{journal: Journal/conference name.}
#' \item{jnl_top_25p: 0/1 variable of whether the journal belongs to the top 25\% in terms of average cites.}
#' \item{jnl_top_5p: 0/1 variable of whether the journal belongs to the top 5\% in terms of average cites.}
#'
#' }
#'
#' @source
#' The source is Microsoft Academic Graph (see reference).
#'
#' @references
#' Arnab Sinha, Zhihong Shen, Yang Song, Hao Ma, Darrin Eide, Bo-June (Paul) Hsu, and Kuansan Wang. 2015. An Overview of Microsoft Academic Service (MAS) and Applications. In Proceedings of the 24th International Conference on World Wide Web (WWW ’15 Companion). ACM, New York, NY, USA, 243-246.
#'
#'
#'
"us_pub_biology"


