#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Sep 30 10:09:06 2019
# ~: user-visible visualization functions
#----------------------------------------------#

# load("data/us_pub_econ.RData")
# roxygen2::roxygenise(roclets = "rd")


#' Plot distributions, possibly conditional
#'
#' This function plots distributions of items (a bit like an histogram) which can be easily conditioned over.
#'
#' @param fml A formula or a vector. If a formula, it must be of the type: \code{var ~ moderator | weight}. If there are no moderator nor weights, you can use directly a vector, or use \code{fml = var ~ 1}. To use weights and no moderator, use \code{fml = var ~ 1 | weight}. See examples.
#' @param data A data.frame: data set containing the variables in the formula.
#' @param moderator Optional, only if argument \code{fml} is a vector. A vector of moderators.
#' @param weight Optional, only if argument \code{fml} is a vector. A vector of (positive) weights.
#' @param maxFirst Logical: should the first elements displayed be the most frequent? By default this is the case except for numeric values put to log or to integers.
#' @param toLog Logical, only used when the data is numeric. If \code{TRUE}, then the data is put to logarithm beforehand. By default numeric values are put to log if the log variation exceeds 3.
#' @param col A vector of colors, default is close to paired. You can also use \dQuote{set1} or \dQuote{paired}.
#' @param outCol Outer color of the bars. Defaults is \code{"black"}.
#' @param maxBins Maximum number of items displayed. The default depends on the number of moderator cases. When there is no moderator, the default is 15, augmented to 20 if there are less than 20 cases.
#' @param bin.size Only used for numeric values. If provided, it creates bins of observations of size \code{bin.size}. It creates bins by default for numeric non-integer data.
#' @param legend_options A list. Other options to be passed to \code{legend} which concerns the legend for the moderator.
#' @param yaxis.show Whether the y-axis should be displayed, default is \code{TRUE}.
#' @param yaxis.num Whether the y-axis should display regular numbers instead of frequencies in percentage points. By default it shows numbers only when the data is weighted with a different function than the sum. For conditionnal distributions, a numeric y-axis can be displayed only when \code{mod.method = "sideTotal"}, \code{mod.method = "splitTotal"} or \code{mod.method = "stack"}, since for the within distributions it does not make sense (because the data is rescaled for each moderator).
#' @param mod.select Which moderators to select. By default the top 3 moderators in terms of frequency (or in terms of weight value if there's a weight) are displayed. If provided, it must be a vector of moderator values whose length cannot be greater than 5. Alternatively, you can put an integer between 1 and 5.
#' @param mod.method A character scalar: either \dQuote{splitWithin}, the default for categorical data, \dQuote{splitTotal}, \dQuote{sideWithin}, the default for data in logarithmic form or numeric data, \dQuote{sideTotal} or \dQuote{stack}. This is only used when there is more than one moderator. If within: the bars represent the distribution within each moderator class; if total, the heights of the bar represent the share in the total distribution. If split: there is one separate histogram for each moderator case. If side: moderators are represented side by side for each value of the variable. If stack: the bars of the moderators are stacked onto each other, the bar heights representing the distribution in the total population (in this case the within distribution does not make sense).
#' @param labels.tilted Whether there should be tilted labels. Default is \code{FALSE} except when the data is split by moderators (see \code{mod.method}).
#' @param labels.angle Only if the labels of the x-axis are tilted. The angle of the tilt.
#' @param addOther Logical. Should there be a last column counting for the observations not displayed? Default is \code{TRUE} except when the data is split.
#' @param plot Logical, default is \code{TRUE}. If \code{FALSE} nothing is plotted, only the data is returned.
#' @param sep Positive number. The separation space between the bars. The scale depends on the type of graph.
#' @param centered Logical, default is \code{TRUE}. For numeric data only and when \code{maxFirst=FALSE}, whether the histogram should be centered on the mode.
#' @param weight.fun A function, by default it is \code{sum}. Aggregate function to be applied to the weight with respect to variable and the moderator. See examples.
#' @param tick_5 Logical. When plotting categorical variables, adds small discrete ticks every 5 bars, and bigger ticks every 10 bars. Helps to get the rank of the bars. By default it is equal to TRUE when strictly more than 10 bars have to be plotted.
#' @param dict A dictionnary to rename the variables names in the axes and legend. Should be a named vector. By default it s the value of \code{getFplot_dict()}, which you can set with the function \code{\link[fplot]{setFplot_dict}}.
#' @param mod.title Character scalar. The title of the legend in case there is a moderator. By default it is equal to the moderator name (possibly modified by the argument dict) if the moderator is numeric, and empty if the moderator is *not* numeric. You can set it to \code{TRUE} to display the moderator name. To display no title, set it to \code{NULL} or \code{FALSE}.
#' @param cex.axis Cex value to be passed to biased labels. By defaults, it finds automatically the right value.
#' @param int.categorical Logical. Whether integers should be treated as categorical variables. By default they are treated as categorical only when their range is small (i.e. smaller than 1000).
#' @param trunc If the main variable is a character, its values are truncaded to \code{trunc} characters. Default is 20. You can set the truncation method with the argument \code{trunc.method}.
#' @param trunc.method If the elements of the x-axis need to be truncated, this is the truncation method. It can be "auto", "trimRight" or "trimRight".
#' @param cumul Logical, default is \code{FALSE}. If \code{TRUE}, then the cumulative distribution is plotted.
#' @param onTop What to display on the top of the bars. Can be equal to "frac" (for shares), "nb" or "none". The default depends on the type of the plot. To disable it you can also set it to \code{FALSE} or the empty string.
#' @param ... Other elements to be passed to plot.
#'
#' @author Laurent Berge
#'
#'
#' @examples
#'
#' # Data on publications from U.S. institutions
#' data(us_pub_econ)
#'
#' # 1) Let's plot the distribution of publications by institutions:
#' plot_distr(institution~1, us_pub_econ)
#'
#' # When there is only the variable, you can use a vector instead:
#' plot_distr(us_pub_econ$institution)
#'
#' # 2) Now the production of institution weighted by journal quality
#' plot_distr(institution ~ 1 | jnl_top_5p, us_pub_econ)
#'
#' # 3) Let's plot the journal distribution for the top 3 institutions
#'
#' # We can get the data from the previous graph
#' graph_data = plot_distr(institution ~ 1 | jnl_top_5p, us_pub_econ, plot = FALSE)
#' # And then select the top universities
#' top3_instit = graph_data$x[1:3]
#' top5_instit = graph_data$x[1:5] # we'll use it later
#'
#' # Now the distribution of journals
#' plot_distr(journal ~ institution, us_pub_econ[institution %in% top3_instit])
#' # Alternatively, you can use the argument mod.select:
#' plot_distr(journal ~ institution, us_pub_econ, mod.select = top3_instit)
#'
#' # 3') Same graph as before with "other" column, 5 institutions
#' plot_distr(journal ~ institution, us_pub_econ,
#'            mod.select = top5_instit, addOther = TRUE)
#'
#' #
#' # Example with continuous data
#' #
#'
#' # regular histogram
#' plot_distr(iris$Sepal.Length)
#'
#' # now splitting by species:
#' plot_distr(Sepal.Length ~ Species, iris)
#'
#' # idem but the three in the same axis:
#' plot_distr(Sepal.Length ~ Species, iris, mod.method = "sideWithin")
#'
#'
#'
plot_distr = function(fml, data, moderator, weight, maxFirst, toLog, maxBins, bin.size, legend_options=list(), onTop, yaxis.show=TRUE, yaxis.num, col, outCol = "black", mod.method, mod.select, tick_5, labels.tilted, addOther, cumul = FALSE, plot = TRUE, sep, centered = TRUE, weight.fun, int.categorical, dict = getFplot_dict(), mod.title, labels.angle, cex.axis, trunc = 20, trunc.method = "auto", ...){
    # This function plots frequencies

    # DT VARS
    total_moderator = x_nb = isOther = otherValue = nb_new = share = share_top = moderator_nb = xleft = xright = ybottom = xleft_real = xright_real = x_num = mid_point = NULL
    ytop_new = ybottom_new = share_top_cum = value_cum = ytop_cum = value = NULL

    # save full formula
    fml_in = fml

    if(!missing(mod.method)){
        mod.method = control_variable(mod.method, "singleCharacterMatch.arg", charVec = c("sideWithin", "sideTotal", "splitWithin", "splitTotal", "stack"))
    }


    if(!missnull(onTop)){
        if(isFALSE(onTop)){
            onTop = "none"
        } else if(length(onTop) == 1 && is.character(onTop) && onTop == ""){
            onTop = "none"
        } else {
            onTop = control_variable(onTop, "singleCharacterMatch.arg", charVec = c("frac", "nb", "none"))
        }

    }

    check_arg(sep, "nullSingleNumericGE0")
    check_arg(maxFirst, "nullSingleLogical")
    check_arg(toLog, "nullSingleLogical")
    check_arg(labels.tilted, "nullSingleLogical")
    check_arg(plot, "singleLogical")
    check_arg(centered, "singleLogical")
    check_arg(maxBins, "nullSingleIntegerGT0")
    check_arg(col, "vector")
    check_arg(bin.size, "nullSingleNumericGT0")
    check_arg(moderator, "nullVector")
    check_arg(weight, "nullNumericVector")
    check_arg(dict, "nullCharacterVectorNoNA")
    control_variable(mod.title, "single(Character|logical)")
    check_arg(labels.angle, "singleNumeric")
    check_arg(yaxis.show, "singleLogical")
    check_arg(yaxis.num, "singleLogical")
    check_arg(int.categorical, "singleLogical")
    check_arg(cumul, "singleLogical")
    check_arg(tick_5, "singleLogical")

    mc = match.call()


    #
    # Extracting x and the moderator:
    #

    USE_WEIGHT = FALSE
    x_name = ylab = weight_name = moderator_name = ""
    if("formula" %in% class(fml_in)){
        if(missing(data) || !is.data.frame(data)){
            postfix = ifelse(!is.data.frame(data), paste0(" Currently it is of class ", enumerate_items(class(data))), "")

            stop("If you provide a formula, a data.frame must be given in the argument 'data'.", postfix)
        }

        vars = all.vars(fml_in)
        if(any(!vars %in% names(data))){
            stop("The variable", enumerate_items(setdiff(vars, names(data)), "s.is")," not in the data set (", deparse(mc$data), ").")
        }

        # Creation of x and the condition
        fml = fml_in
        if(!length(fml) == 3){
            stop("The formula must be of the type value ~ moderator | weight.")
        }

        fml = extract_pipe(fml_in)$fml
        pipe = extract_pipe(fml_in)$pipe

        x = eval(fml[[2]], data)
        moderator = eval(fml[[3]], data)
        weight = eval(pipe, data)

        if(is.null(weight)){
            weight = rep(1, length(x))
        } else {
            USE_WEIGHT = TRUE
            weight_name = gsub("^.*\\| *", "", deparse(fml_in[[3]]))
            check_arg(weight, "numericVectorGE0", "The 'weight' must be a numeric vector. REASON")
        }

        if(length(moderator) == 1){
            moderator = rep(1, length(x))
        }

        # other info
        x_name = deparse(fml[[2]])
        moderator_name = deparse(fml[[3]])
    } else {
        x = fml_in

        # default no name when no formula => otherwise ugly when exporting
        x_name = clean_name(deparse(substitute(fml)))

        # We check the vector
        if(!checkVector(x)){

            if(length(class(x)) == 1 && class(x) == "table"){
                x = as.vector(x)
            } else {
                if(length(x) == 0){
                    reason = "Currently, the length of fml is 0."
                } else {
                    reason = paste0("Currently, fml is of class ", enumerate_items(class(x)), ".")
                }

                stop("Wrong argument in fml. It must be either a formula, either a vector. ", reason)
            }

        }

        # moderator
        if(!missnull(moderator)){
            if(length(x) != length(moderator)){
                stop("The arguments 'x' and 'moderator' must be of the same length.")
            }
            moderator_name = clean_name(deparse(substitute(moderator)))
        } else {
            moderator = rep(1, length(x))
        }

        # weight
        if(!missnull(weight)){
            if(length(x) != length(weight)){
                stop("The arguments 'x' and 'weight' must be of the same length.")
            }
            control_variable(weight, "numericVectorGE0")
            USE_WEIGHT = TRUE
            weight_name = clean_name(deparse(substitute(weight)))
        } else {
            weight = rep(1, length(x))
        }
    }

    # Renaming: dict
    x_name = getNames(x_name, dict)
    moderator_name = getNames(moderator_name, dict)
    weight_name = getNames(weight_name, dict)

    # Dropping NAs
    quiNA_x = is.na(x)
    quiNA_mod = is.na(moderator)
    quiNA_weight = is.na(weight)
    quiNA = quiNA_x | quiNA_mod | quiNA_weight
    if(any(quiNA)){
        nb_na = c(sum(quiNA_x), sum(quiNA_mod), sum(quiNA_weight))
        msg_na = paste0(c("x: ", "moderator: ", "weight: "), nb_na)
        message("NOTE: ", sum(quiNA), " observations with NAs (", enumerate_items(msg_na[nb_na>0]), ")")
        x = x[!quiNA]
        moderator = moderator[!quiNA]
        weight = weight[!quiNA]
    }

    # case of empty strings
    if(is.character(x)){
        qui_empty = x == ""
        if(any(qui_empty)){
            x[qui_empty] = "[empty]"
        }
    }

    # Dealing with the moderator
    if(is.factor(moderator)){
        moderator_unik = levels(moderator[drop = TRUE])
    } else {
        moderator_unik = sunique(moderator)
    }

    if(cumul){
        if(!missing(addOther) && addOther == TRUE) message("Argument addOther = TRUE is ignored since the cumulative distribution is asked for.")
        addOther = FALSE
    }

    moderator_cases = length(moderator_unik)
    isLegend = FALSE
    USE_MOD = FALSE

    if(moderator_cases > 1){
        USE_MOD = TRUE
        isLegend = TRUE

        n_select = NA
        do_recreate = FALSE
        if(missing(mod.select)){
            if(moderator_cases <= 5){
                # FINE!
            } else {
                # We select the top 3
                n_select = 3
            }
        } else if(length(mod.select) > 5){
            stop("The argument 'mod.select' (currently of length ", length(mod.select), ") cannot be of a length greater than 5!")
        } else if(length(mod.select) == 1 && is.numeric(mod.select) && mod.select %% 1 == 0){
            # Possibly a number
            if(mod.select %in% 1:5){
                n_select = mod.select
            } else {
                stop("Argument 'mod.select' does not have a valid value: if it is an integer, its value must lie between 1 and 5.")
            }
        } else {
            # mode.select must contain moderator values

            # 1) checking the problems
            mod_pblm = setdiff(mod.select, moderator_unik)
            if(length(mod_pblm) > 0){

                if(length(mod_pblm) != length(mod.select) && any(quiNA)){
                    # means some are OK
                    suggestion = paste0(" Maybe because ", ifsingle(mod_pblm, "it has", "they have"), " only NA values?")
                } else {
                    suggestion = " It must consist of a vector of moderator values only."
                }

                stop("In the argument 'mod.select', the value", enumerate_items(mod_pblm, "s.is"), " not in the moderator variable.", suggestion)
            }

            # 2) selection
            do_recreate = TRUE

        }

        # AUTOMATIC selection
        if(!is.na(n_select) && moderator_cases > n_select){

            # We proceed with the "automatic" selection

            if(USE_WEIGHT){
                info_mod = data.table(moderator = moderator, w = weight)
                info_mod = info_mod[, list(value = sum(w)), by = moderator]
                info_method = "the value of the weight."
            } else {
                info_mod = data.table(moderator = moderator)
                info_mod = info_mod[, list(value = .N), by = moderator]
                info_method = "frequency."
            }

            # We inform on the method for the choice
            message(ifelse(n_select == 1, "The moderator was", paste0("The ", n_select, " moderators were")), " chosen based on ", info_method)

            info_mod = info_mod[order(-value)]
            mod.select = info_mod[1:n_select, moderator]

            do_recreate = TRUE

            if(n_select == 1){
                USE_MOD = isLegend = FALSE
            }

        }

        if(do_recreate){
            # We recreate the variables

            qui_select = which(moderator %in% mod.select)
            # we recreate all the values
            x = x[qui_select]
            moderator = moderator[qui_select]
            weight = weight[qui_select]

            # Re-Dealing with the moderator
            if(is.factor(moderator)){
                moderator_unik = levels(moderator[drop = TRUE])
            } else {
                moderator_unik = sunique(moderator)
            }

            moderator_cases = length(moderator_unik)
        }

    }

    # if(moderator_cases > 5) {
    #     stop("The number of cases of the moderator cannot be greater than 5!")
    # } else if(moderator_cases > 1){
    #     USE_MOD = TRUE
    #     # we put the legend only if there is a condition
    #     isLegend = TRUE
    # }

    # mod.title
    if(isLegend){
        if(missing(mod.title)){
            if(is.numeric(moderator) || is.logical(moderator)){
                mod.title = moderator_name
            } else {
                mod.title = NULL
            }
        } else if(isTRUE(mod.title)){
            mod.title = moderator_name
        } else if(isFALSE(mod.title)){
            mod.title = NULL
        }
    }


    # separation of the x cases:
    if(missnull(sep)){
        sep = 0.20 + 0.15 * (moderator_cases-1)
    }

    # Setting up the data
    # Finding out whether log or not
    isNum = is.numeric(x) && !USE_WEIGHT # no sense to weight "real" numeric data

    # Default of toLog and maxFirst
    delayLogChecking = FALSE
    if(missnull(toLog)){
        if(!isNum || any(x < 0)){
            toLog = FALSE
        } else {
            toLog = FALSE
            if(diff(range(log(x[x>0]))) >= 3 && max(x) > 100){
                delayLogChecking = TRUE
            }
        }
    } else if(toLog && !isNum){
        warning("Since 'x' is not numeric, argument 'toLog' is ignored.")
        toLog = FALSE
    }

    isInteger = NA
    if(missnull(maxFirst)){
        if(!isNum){
            maxFirst = TRUE
        } else if(toLog){
            maxFirst = FALSE
        } else {
            isInteger = all(x %% 1 == 0)
            if(isInteger){
                maxFirst = FALSE
            } else {
                maxFirst = FALSE
            }
        }
    }

    IS_MAXBIN_USER = TRUE
    if(missnull(maxBins)){
        IS_MAXBIN_USER = FALSE
        bin_fit_all = c(20, 11, 8, 6, 5)
        bin_max_all = c(15, 9, 7, 5, 4)

        nb = ifelse(!missing(mod.method) && mod.method == "stack", 1, moderator_cases)

        maxBins = bin_fit_all[nb]
        if(length(unique(x)) > maxBins){
            maxBins = bin_max_all[nb]
        }
    }

    if(toLog){
        # Regle: la valeur d'une colonne est: superieur ou egal a la valeur gauche et strictement inferieurs a la valeur droite
        x = pmax(floor(log(x + 1e-6)), -1)
    }

    numAxis = FALSE
    binned_data = FALSE
    maxBins_old = maxBins # used with delayLogChecking
    if(isNum && !toLog){
        if(!missnull(bin.size)){
            if(!missing(int.categorical) && int.categorical && all(x %% 1 == 0)){
                isInteger = TRUE
            } else {
                x = (x %/% bin.size) * bin.size
                binned_data = TRUE
                if(!maxFirst){
                    numAxis = TRUE
                    if(IS_MAXBIN_USER == FALSE){
                        # Since it's a numeric axis, we put a LOT of bins
                        # It is a maximum anyway
                        maxBins = 50
                    }
                }
            }

        } else {
            if(is.na(isInteger)){
                isInteger = all(x %% 1 == 0)
            }

            goNum = TRUE
            if(!missing(int.categorical)){
                # we force integers as categorical
                if(int.categorical == TRUE) goNum = FALSE
                # if int.categorical == FALSE, then we force it as numeric
            } else if(isInteger && diff(range(x)) < 1000){
                # for integers with small range, we don't consider them
                # as numeric
                goNum = FALSE
            }

            if(goNum){

                if(!maxFirst){
                    numAxis = TRUE
                }

                if(IS_MAXBIN_USER == FALSE){
                    # we reset the number of bins: specific to the numeric case
                    maxBins = bin_max_all[1]
                }

                binned_data = TRUE

                # we try to find a "good" bin size
                x_min = min(x)
                x_max = max(x)
                bin.size = signif((x_max - x_min) / min(max(maxBins - 1, 10), 15), 1)
                x_tmp = (x %/% bin.size) * bin.size
                tx = ttable(x_tmp)
                if(sum(tx/sum(tx) > 0.03) < 6){
                    # Condition: nber of bins with more than 3% is lower than 6

                    if(delayLogChecking){
                        # Would it be better looking in logarithmic form?
                        x_ln = pmax(floor(log(x + 1e-6)), -1)
                        tx_ln = ttable(x_ln)
                        if(sum(tx_ln/sum(tx_ln) > 0.05) >= 5){
                            # nber of signif bins greater than 5
                            toLog = TRUE
                            x = x_ln
                            numAxis = FALSE
                            binned_data = FALSE
                            maxBins = maxBins_old
                        }

                    }
                }

                if(!toLog){
                    if(sum(tx/sum(tx) > 0.01) < 6){
                        # Condition: nber of bins with more than 1% is lower than 6
                        # More complex binning:
                        # We try to find the "optimal" number of bins

                        n_bins = maxBins
                        q001 = quantile(x, seq(0, 1, by = 1 / 40))
                        n_q = length(q001)

                        s_vect = c()
                        score_total_vect = c()
                        score_range_vect = c()
                        score_fullness_vect = c()

                        for(i in 1:20){

                            if(i == 1){
                                s = signif(diff(range(x)) / n_bins, 1)
                            } else {
                                s = signif(s / 1.5, 1)
                            }

                            value_range = integer(n_bins)
                            for(k in 1:n_q){
                                s_start = q001[k]
                                value_range[k] = sum(q001 >= s_start & q001 <= s_start + n_bins*s)
                                if(value_range[k] == length(q001)) break
                            }
                            s_start = q001[which.max(value_range)]

                            missing_range = 1 - sum(q001 >= s_start & q001 <= s_start + n_bins*s)/n_q
                            score_fullness = length(unique(na.omit(cut(q001, seq(s_start, by = s, length.out = n_bins + 1), include.lowest = TRUE))))/ n_bins

                            # Save
                            s_vect[i] = s
                            score_total_vect[i] = score_fullness/2 - 2*missing_range

                            if(missing_range > 0.4) break
                        }

                        bin.size = s_vect[which.max(score_total_vect)]
                        x = (x %/% bin.size) * bin.size
                    } else {
                        x = x_tmp
                    }

                }
            }

        }

    }

    # Splitting
    if(missnull(mod.method)){
        if(toLog || numAxis){
            mod.method = "sideWithin"
        } else {
            mod.method = "splitWithin"
        }
    }

    DO_STACK = FALSE
    if(mod.method == "stack"){

        if(cumul == TRUE){
            stop("You cannot use the argument cumul=TRUE with mod.method='stack' (it makes no sense).")
        }

        DO_STACK = TRUE
        mod.method = "sideTotal"
    }

    DO_SPLIT = FALSE
    checkNotTilted = FALSE
    delayLabelsTilted = missnull(labels.tilted)
    if(moderator_cases > 1 && grepl("split", mod.method)){
        DO_SPLIT = TRUE
        if(missnull(labels.tilted)){
            labels.tilted = TRUE
            checkNotTilted = TRUE
        }

        # We don't add the other column only when the data is split
        # and it is not a numeric axis or log axis
        if(missnull(addOther) && !toLog && !numAxis){
            addOther = FALSE
        }
    }

    if(missnull(addOther)){
        addOther = TRUE
    }

    # yaxis.num
    if(missnull(yaxis.num)){
        if(USE_WEIGHT == FALSE || missing(weight.fun) || (moderator_cases > 1 && grepl("ithin", mod.method))){
            yaxis.num = FALSE
        } else {
            yaxis.num = TRUE
        }
    } else if(yaxis.num == TRUE && moderator_cases > 1 && grepl("ithin", mod.method)){
        stop("Argument yaxis.num: You cannot have a numeric y-axis when the 'within' distributions among moderators should be reported (because the data is rescaled as frequencies first). Use mod.method = 'sideTotal', mod.method = 'splitTotal' or mod.method = 'stack' to display a numeric y-axis.")
    }

    checkForTilting = FALSE
    if(missnull(labels.tilted)){
        labels.tilted = FALSE
        # We check if it's better for tilting only in the regular case
        checkForTilting = TRUE
        # NOTE that we later update the value of checkForTilting
        # because we don't want to do it all the time (but we first need more information)
        # just search the next occurrent of checkForTilting
    }

    # The color
    if(missnull(col)){
        if(moderator_cases == 1){
            col = "#1F78B4"
        } else if(DO_SPLIT){
            col = "set1"
        } else {
            col = c("#1F78B4", "#A6CEE3", "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#FF7F00", "#FDBF6F")
        }
    }

    if(length(col) == 1 && is.character(col)){
        col_match = try(match.arg(tolower(col), c("set1", "paired")), silent = TRUE)
        if(col_match == "set1"){
            col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
        } else if(col_match == "paired"){
            col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00")
        }
    }

    numLabel = FALSE
    if(moderator_cases >= 2 && numAxis){

        if(DO_SPLIT){
            numLabel = TRUE
        }

        sep = 0
    }

    # for table elements
    if(is.table(x)){
        x = as.vector(x)
    }

    X_FACTOR = FALSE
    if(is.factor(x)){
        X_FACTOR = TRUE
        # we keep the order of x when factors
        x = x[drop = TRUE]
        x_fact_names = levels(x)
        x = unclass(x)
        # => we rename x later
    }

    #
    # Computing the frequency => data_freq
    #

    ADD_OTHER = ADD_OTHER_LEFT = CUT_BAR = FALSE

    if(DO_SPLIT == FALSE){

        if(USE_WEIGHT){
            raw_data = data.table(x = x, weight = weight, moderator = moderator)
            if(missnull(weight.fun)){
                data_freq = raw_data[, list(value = sum(weight)), keyby = list(x, moderator)]
            } else {
                if(!is.function(weight.fun)){
                    stop("Argument 'weight.fun' must be a function.")
                } else if(length(weight.fun(head(weight))) != 1){
                    stop("Function 'weight.fun' must be return a value of length one.")
                }

                data_freq = raw_data[, list(value = weight.fun(weight)), keyby = list(x, moderator)]
            }

        } else {
            raw_data = data.table(x = x, moderator = moderator)
            data_freq = raw_data[, list(value = .N), keyby = list(x, moderator)]
        }

        if(USE_MOD && mod.method == "sideWithin"){
            data_freq[, "total_moderator" := list(sum(value)), by = moderator]
            data_freq[, "share" := list(value/total_moderator*100)]
        } else {
            total = sum(data_freq$value)
            data_freq[, "share" := list(value/total*100)]
        }

        # We balance the panel for log data
        if(toLog && maxFirst == FALSE){
            x_all = min(data_freq$x):max(data_freq$x)
            data_all = data.table(expand.grid(x = x_all, moderator = moderator_unik))
            tmp = merge(data_all, data_freq, all.x = TRUE, by = c("x", "moderator"))
            tmp[is.na(tmp)] = 0
            data_freq = tmp

        }


        # The sorting
        if(maxFirst){

            if(USE_MOD){
                data_freq[, "total_x" := list(sum(value)), by = x]
                # data_freq = data_freq[order(-total_x, moderator)]
                # setorderv(data_freq, c("total_x", "moderator", "x"), c(-1, 1, 1))
                setorderv(data_freq, c("total_x", "x"), c(-1, 1))
            } else {
                # data_freq = data_freq[order(-value)]
                setorderv(data_freq, "value", -1)
            }
            data_freq[, "x_nb" := list(rleid(x))]

        } else {
            data_freq = data_freq[order(x, moderator)]
            data_freq[, x_nb := quickUnclassFactor(x)]
        }

        # the "other" column
        data_freq[, isOther := FALSE]
        data_freq[, otherValue := ""]


        # if we center on the mode
        if(centered && any(data_freq$x_nb > maxBins) && isNum){
            # we center the distribution
            data_freq[, nb_new := x_nb - which.max(value)]
            data_freq[, nb_new := nb_new + max(min(ceiling(maxBins/2), abs(min(nb_new)) + 1), maxBins - max(nb_new))]
            data_freq[, x_nb := nb_new]
            data_freq[, nb_new := NULL]

            if(addOther && maxFirst == FALSE && any(data_freq$x_nb <= 0)){
                if(!any(data_freq$x_nb < 0)){
                    # we just add that bin
                    data_freq[, x_nb := x_nb + 1]
                    maxBins = maxBins + 1
                } else {
                    ADD_OTHER_LEFT = TRUE
                    # We create a bin on the left
                    data_other_left = data_freq[x_nb <= 0]
                    data_other_left = data_other_left[, list(value = sum(value), share_top = sum(share)), by = moderator]
                    tmp = as.data.table(expand.grid(moderator = data_other_left$moderator))
                    other_col = merge(data_other_left, tmp)
                    other_col[is.na(other_col)] = 0
                    other_col[, x_nb := 0]
                    # we limit the size of this last column
                    share_max = max(data_freq[x_nb %in% 1:maxBins, share])
                    # r = 1 + rank(other_col$value, ties.method = "max") / moderator_cases / 10
                    # other_col[, share := pmin(share_top, r * share_max)]
                    other_col[, share := pmin(share_top, 1.1 * share_max)]

                    if(any(other_col$share < other_col$share_top)){
                        CUT_BAR = TRUE
                    }

                    # we merge the information
                    data_freq = rbindDS(other_col, data_freq[x_nb > 0])
                    qui_NA = is.na(data_freq$share_top)
                    data_freq$share_top[qui_NA] = data_freq$share[qui_NA]

                    # Other text => depends on the type of values
                    data_freq[x_nb == 0, "isOther" := TRUE]
                    if(toLog){
                        x_first = ceiling(exp(data_freq[x_nb == 1, x][1]))
                        otherTextLeft = substitute(phantom()<x_val, list(x_val = formatAxisValue(x_first)))
                        data_freq[x_nb == 0, "otherValue" := paste("<", formatAxisValue(x_first))]
                    } else {
                        if(binned_data == FALSE){
                            x_other = data_freq[x_nb == 1, x][1]
                            otherTextLeft = substitute(phantom()<x_val, list(x_val = formatAxisValue(x_other)))
                            data_freq[x_nb == 0, "otherValue" := paste("<", formatAxisValue(x_other))]
                        } else {
                            # we update the value
                            x_other = data_freq[x_nb == 1, x][1]
                            data_freq[x_nb == 0, x := x_other - bin.size]
                            otherTextLeft = substitute(phantom()<x_other, list(x_other = formatAxisValue(x_other)))
                            data_freq[x_nb == 0, "otherValue" := paste("<", formatAxisValue(x_other))]
                        }

                    }
                }
            } else {
                # We clean negative numbers
                data_freq = data_freq[x_nb > 0]
            }

        }

        # The "other" column
        if(addOther && max(data_freq$x_nb) == maxBins + 1){
            # we just add the bin
            maxBins = maxBins + 1
            data_freq = data_freq[x_nb <= maxBins]
        } else if(addOther && any(data_freq$x_nb > maxBins)){
            # we create the other colum as the sum of the rest
            ADD_OTHER = TRUE
            data_other = data_freq[x_nb > maxBins]
            data_other = data_other[, list(value = sum(value), share_top = sum(share)), by = moderator]
            tmp = as.data.table(expand.grid(moderator = data_other$moderator))
            other_col = merge(data_other, tmp)
            other_col[is.na(other_col)] = 0
            other_col[, x_nb := maxBins + 1]
            # we limit the size of this last column
            share_max = max(data_freq[x_nb %in% 1:maxBins, share])

            # r = 1 + rank(other_col$value, ties.method = "max") / moderator_cases / 10
            # other_col[, share := pmin(share_top, r * share_max)]
            other_col[, share := pmin(share_top, 1.1 * share_max)]

            if(any(other_col$share < other_col$share_top)){
                CUT_BAR = TRUE
            }

            # we merge the information
            data_freq = rbindDS(data_freq[x_nb <= maxBins], other_col)
            qui_NA = is.na(data_freq$share_top)
            data_freq$share_top[qui_NA] = data_freq$share[qui_NA]

            # Other text => depends on the type of values
            data_freq[x_nb == maxBins + 1, "isOther" := TRUE]
            if(!isNum || maxFirst){
                otherText = "Other"
                data_freq[x_nb == maxBins + 1, "otherValue" := "Other"]
            } else {
                if(toLog){
                    x_last = round(exp(data_freq[x_nb == maxBins, x] + 1))
                    otherText = substitute(phantom()>x_val, list(x_val = formatAxisValue(x_last)))
                    data_freq[x_nb == maxBins + 1, "otherValue" := paste(">", formatAxisValue(x_last))]
                } else {
                    if(binned_data == FALSE){
                        x_value = data_freq[x_nb == maxBins, x]
                        otherText = substitute(phantom()>x_val, list(x_val = formatAxisValue(x_value)))
                        data_freq[x_nb == maxBins + 1, "otherValue" := paste(">", formatAxisValue(x_value))]
                    } else {
                        x_other = data_freq[x_nb == maxBins, x][1] + bin.size
                        data_freq[x_nb == maxBins + 1, x := x_other]
                        otherText = substitute(phantom()>=x_other, list(x_other = formatAxisValue(x_other)))
                        data_freq[x_nb == maxBins + 1, "otherValue" := paste(">", formatAxisValue(x_other))]
                    }

                }
            }

        } else {
            data_freq = data_freq[x_nb <= maxBins]
        }



        if(!"share_top" %in% names(data_freq)){
            data_freq[, share_top := share]
        }

        x_nb_all = sort(unique(data_freq$x_nb))
        x_cases = length(x_nb_all)

        # We add the 0s
        if(nrow(data_freq) != x_cases*moderator_cases){
            # we need to add the 0s
            data_all = data.table(expand.grid(x_nb = x_nb_all, moderator = moderator_unik))
            setkey(data_all, x_nb, moderator)
            setkey(data_freq, x_nb, moderator)

            tmp = merge(data_all, data_freq, all.x = TRUE)
            tmp[, c("x", "isOther", "otherValue") := NULL]

            # Merging additional info
            # x_unik_all = unique(data_freq$x)
            # x_nb_unik_all = unique(data_freq$x_nb)
            # tmp$x[is.na(tmp$x)] = x_unik_all[dict2number(x_nb_unik_all, tmp$x_nb[is.na(tmp$x)])]
            base2merge = unique(data_freq[, list(x, x_nb, isOther, otherValue)])
            quoi = merge(tmp, base2merge, by = "x_nb")
            tmp = quoi[order(x_nb, moderator)]

            # setting to 0
            tmp[is.na(tmp)] = 0
            data_freq = tmp
        }


        # moderator_nb
        if(is.factor(moderator)){
            data_freq[, moderator_nb := as.numeric(unclass(moderator[drop = TRUE]))]
        } else {
            data_freq[, moderator_nb := quickUnclassFactor(moderator)]
        }

        # The coordinates
        if(numAxis){
            data_freq[, xleft := x + bin.size/moderator_cases * (moderator_nb - 1)]
            data_freq[, xright := x + bin.size/moderator_cases * moderator_nb]
        } else {
            data_freq[, xleft := (x_nb-1)*(moderator_cases+sep) + moderator_nb - 1]
            data_freq[, xright := (x_nb-1)*(moderator_cases+sep) + moderator_nb]
        }

        xlim = range_plus(c(data_freq$xleft, data_freq$xright), 5)

        data_freq[, ybottom := 0]
        data_freq[, ytop := share]

        if(X_FACTOR){
            # we reintroduce the values of x

            # beware x == 0 means Other!!!
            x_value = rep("Other", nrow(data_freq))
            qui_ok = data_freq$x != 0
            x_value[qui_ok] = x_fact_names[data_freq$x[qui_ok]]
            x_value[!qui_ok] = NA
            data_freq[, x := x_value]
            # data_freq[, x := x_fact_names[x]]
        }

    } else if(DO_SPLIT == TRUE){
        # We apply a split to the data
        info = list()
        x_add = 0

        for(i in seq_along(moderator_unik)){
            # we apply plot_distr to subsets

            qui = moderator == moderator_unik[i]
            if(USE_WEIGHT){
                data_freq = plot_distr(x[qui], moderator = moderator[qui], weight = weight[qui], maxFirst = maxFirst, maxBins = maxBins, toLog = FALSE, addOther = addOther, plot = FALSE, bin.size = bin.size, int.categorical = !numAxis)
            } else {
                data_freq = plot_distr(x[qui], moderator = moderator[qui], maxFirst = maxFirst, maxBins = maxBins, toLog = FALSE, addOther = addOther, plot = FALSE, bin.size = bin.size, int.categorical = !numAxis)
            }

            # updating the information

            if(numAxis){
                # We add the value of a bin, BUT start when the last stops

                # we save the "real" values
                data_freq[, xleft_real := xleft]
                data_freq[, xright_real := xright]

                # Now we update xleft/xright
                min_left = min(data_freq$xleft)
                x_add_next = x_add + max(data_freq$xright) - min_left
                data_freq[, xleft := xleft - min_left + x_add]
                data_freq[, xright := xright - min_left + x_add]

                x_add = x_add_next + bin.size
            } else {
                min_left = min(data_freq$xleft)
                x_add_next = x_add + max(data_freq$xright) - min_left
                data_freq[, xleft := xleft - min_left + x_add]
                data_freq[, xright := xright - min_left + x_add]

                x_add = x_add_next + 0.7
            }

            data_freq[, moderator_nb := i]

            info[[i]] = data_freq
        }

        info_all = rbindlist(info, use.names=TRUE)

        if(mod.method == "splitTotal"){
            total = sum(weight)
            info_all[, share := value/total*100]
            info_all[, share_top := share]
            info_all[, ytop := share]
        }

        if(binned_data && addOther){
            # we format the data properly
            info_all$x = formatAxisValue(info_all$x)
        }

        if(X_FACTOR){
            # we reintroduce the values of x
            info_all[, x := x_fact_names[x]]
        }

        # managing "other"
        # info_all[, isOther := is.na(x)]

        if(any(info_all$isOther)){
            # bounding bar size
            share_max = max(info_all[isOther == FALSE, share])
            info_all[isOther == TRUE, share := pmin(share_top, 1.1 * share_max)]
            info_all[, ytop := share]

            if(any(info_all$share < info_all$share_top)){
                CUT_BAR = TRUE
            }

            info_all$x = as.character(info_all$x)
            info_all$x[is.na(info_all$x)] = "Other"
        }

        data_freq = info_all

        # Some information
        xlim = range_plus(c(data_freq$xleft, data_freq$xright), 5)
    }


    if(DO_STACK == TRUE){
        # we reformat the data
        # return(data_freq)
        new_coords = data_freq[isOther == FALSE, list(x_nb, xleft, xright, ybottom, ytop)]

        new_coords[, c("xleft_new", "xright_new") := list(min(xleft), max(xright)), by = x_nb]
        new_coords[, ytop_new := cumsum(ytop), by = x_nb]
        new_coords[, ybottom_new := shift(ytop_new, n = 1, fill = 0), by = x_nb]

        data_freq[isOther == FALSE, c("xleft", "xright", "ybottom", "ytop") := list(new_coords$xleft_new, new_coords$xright_new, new_coords$ybottom_new, new_coords$ytop_new)]

        if(any(data_freq$isOther)){
            # new values top
            max_y = max(data_freq$ytop)
            data_freq[isOther == TRUE, ytop := pmin(1.1 * max_y, share_top)]
        }
        # browser()
    }

    # Cumulative
    if(cumul == TRUE){
        data_freq[, share_top_cum := cumsum(share_top), by = moderator]
        data_freq[, value_cum := cumsum(share), by = moderator]
        data_freq[, ytop_cum := cumsum(ytop), by = moderator]

        data_freq[, share_top := share_top_cum]
        data_freq[, value := value_cum]
        data_freq[, ytop := ytop_cum]
    }

    if(plot == FALSE){
        return(data_freq)
    }

    # Plot preparation
    dots = list(...)

    # default for onTop
    if(missnull(onTop)){
        if(cumul){
            onTop = "frac"
        } else if(numAxis){
            if(any(data_freq$isOther)){
                onTop = "frac"
            } else {
                onTop = "none"
            }
        } else if(moderator_cases > 1 && !grepl("split", mod.method)){
            # when neck to neck => much easier to compare shares
            # otherwise it is confusing
            onTop = "frac"
        } else {
            # if coef of variation of x is low => better information is the number
            values = data_freq[share_top <= share, ytop]
            cv = sd(values) / mean(values)
            if(cv > 0.7){
                onTop = "frac"
            } else {
                onTop = "nb"
            }
        }
    }

    if(is.null(dots$ylim)){
        ylim = c(0, max(data_freq$ytop))
        ymax_grid = ylim[2]

        # hauteur_caractere_legend = 0
        total_height = 0
        if(isLegend){
            info_legend = legendFit(legend = moderator_unik, plot = FALSE, title = mod.title)
            total_height = info_legend$total_height
        }

        hauteur_caractere_top = 0
        if(onTop != "none"){

            # We create the information to display top

            if(DO_STACK == FALSE){
                # Normal case
                info_top = data_freq[, list(x_mid = (xleft+xright)/2, ytop, value, share_top)]
            } else if(DO_STACK == TRUE){
                info_top = data_freq[isOther == FALSE, list(x_mid = mean((xleft+xright)/2), ytop = max(ytop), value = sum(value), share_top = sum(share_top)), by = x_nb]
                info_top_other = data_freq[isOther == TRUE, list(x_mid = (xleft+xright)/2, ytop, value, share_top)]
            }


            # We find the optimal cex for the top numbers
            if(onTop == "nb"){
                top.value2display = formatAxisValue(info_top$value)
            } else {
                top.value2display = paste0(mysignif(info_top$share_top, d = 2, r = 0), "%")
            }

            #
            # NOTE on R behavior with new windows (x11) that are resized
            #
            # The following code computes the optimal width of the number/fractions to be displayed on the top
            # of the bars.
            # However, when we use x11() and then resize the window, R is unable to find out the actual window size
            # Instead we are only able to access the original size.
            # Unfortunately, the drawbacks of the following solutions are too important:
            #   - plotting a "clean" plot, so that R knows the actual size of the window.
            #       * Drawback: if the user calls for several plots in the same window => we're screwed
            #   - making a "smart guess" by assuming that the new window will be larger
            #       * drawback: if it is not the case, it will be ugly (better too small than too big)

            # Getting the width of the bars in inches
            unitary_width = par("pin")[1] / usr_width(xlim)
            if(numAxis){
                tdx = table(data_freq$xright - data_freq$xleft)
                bar_w = as.numeric(names(tdx)[which.max(tdx)])
                unitary_width = unitary_width * bar_w
            }

            # When DO_STACK==TRUE and numAxis==FALSE:
            #   the width is equal to the nber of moderators
            unitary_width_top = unitary_width * (1 + (moderator_cases - 1) * DO_STACK * (!numAxis))

            top.cex = 1
            minCex = 0.5
            v_max = top.value2display[which.max(strwidth(top.value2display, units = "in"))]
            while(top.cex > minCex && strwidth(v_max, units = "in", cex = top.cex) > unitary_width_top*0.9){
                top.cex = top.cex * 0.95
            }

            # the height we need to add to the plot
            hauteur_caractere_top = strheight("W", "in", cex = top.cex) * 1.35 + strwidth("W", "in", cex = 1) * top.cex

            # If DO_STACK==TRUE: the "other" columns have another width
            if(DO_STACK == TRUE && nrow(info_top_other) > 0){

                if(onTop == "nb"){
                    top.value2display_other = formatAxisValue(info_top_other$value)
                } else {
                    top.value2display_other = paste0(mysignif(info_top_other$share_top, d = 2, r = 0), "%")
                }

                top.cex_other = 1
                minCex = 0.5
                v_max = top.value2display_other[which.max(strwidth(top.value2display_other, units = "in"))]
                while(top.cex_other > minCex && strwidth(v_max, units = "in", cex = top.cex_other) > unitary_width*0.9){
                    top.cex_other = top.cex_other * 0.95
                }

                hauteur_caractere_top_other = strheight("W", "in", cex = top.cex_other) * 1.35 + strwidth("W", "in", cex = 1) * top.cex_other

                # We are careful to add only the necessary height to the plot
                hauteur_caractere_top_usr = hauteur_caractere_top / par("pin")[2] * diff(ylim)
                hauteur_caractere_top_other_usr = hauteur_caractere_top_other / par("pin")[2] * diff(ylim)
                height_normal = max(info_top$ytop) + hauteur_caractere_top_usr
                height_other = max(info_top_other$ytop) + hauteur_caractere_top_other_usr

                hauteur_caractere_top = (max(height_normal, height_other) - ylim[2]) * par("pin")[2] / diff(ylim)
            }

        }

        # We transform the character heights into user format (produit en croix)
        add_usr = (hauteur_caractere_top + total_height) / par("pin")[2] * diff(ylim)
        ylim[2] = ylim[2] + add_usr

        dots$ylim = ylim
    }

    dots$xlim = xlim
    dots$axes = FALSE
    dots$x = dots$y = dots$col = 0

    # ylab
    prefix = ""
    if(cumul) prefix = "Cumulative "
    if(moderator_cases > 1){
        text = switch(mod.method, sideWithin = "% Within", sideTotal = "% Total", splitWithin = "% Within", splitTotal = "% Total")

        if(USE_WEIGHT){
            if(yaxis.num == TRUE){
                ylab = paste0(prefix, weight_name)
            } else {
                ylab = paste0("Distribution of ", weight_name, " (", prefix, text, ")")
            }
        } else {
            if(yaxis.num == TRUE){
                ylab = paste0(prefix, "Frequency")
            } else {
                ylab = paste0(prefix, text)
            }
        }
    } else if(USE_WEIGHT){
        ylab = paste0(prefix, "Distribution of ", weight_name)
    } else if(cumul){
        ylab = "Cumulative %"
    }

    # xlab

    # user providing xlab?
    noXlab = is.null(dots$xlab)
    noSub = is.null(dots$sub)
    if(!noXlab || isNum){
        # if data is numeric, we will want the x-label, even if tilted
        max_line = 1
    } else if(!noSub){
        max_line = 2
    } else {
        max_line = 3
    }

    # Update of checkForTilting
    if(checkForTilting){
        # Only for the standard case
        checkForTilting = !numAxis & !toLog
    }

    if(checkForTilting){
        # "at risk" of tilting => we don't show the name
        listDefault(dots, "xlab", "")
    } else if(labels.tilted){
        # we don't show the name of the variable if lab is tilted
        # except if the data is numeric

        if(isNum){
            listDefault(dots, "xlab", x_name)
        } else {
            listDefault(dots, "xlab", "")
        }

        # listDefault(dots, "sub", x_name)
    } else {
        listDefault(dots, "xlab", x_name)
    }


    listDefault(dots, "ylab", ylab)
    listDefault(dots, "yaxs", "i")

    do.call("plot", dots)

    hgrid(ymax = ymax_grid)

    rect(xleft = data_freq$xleft, ybottom = data_freq$ybottom, xright = data_freq$xright, ytop = data_freq$ytop, col = col[data_freq$moderator_nb], border = outCol)

    #
    # "cutting" the bar (if other is very big)
    #

    if(CUT_BAR){
        data_other = data_freq[share_top > share]
        ytop = data_other$ytop[1]
        x_span = (data_other$xright - data_other$xleft)[1] / diff(get_x_lim()) * diff(get_y_lim())
        y_span =  ytop * .03
        y_all = ytop - 2:1*x_span
        for(i in 1:nrow(data_other)){
            shade_area(y_all, y_all - y_span, x = c(data_other$xleft[i], data_other$xright[i]), col = "white", border = "white")
        }
    }

    #
    # "Decorations" => axes
    #

    # Information on plot
    at_info = data_freq[, list(mid_point = (max(xright) + min(xleft)) / 2), by = list(x_nb, x)]
    myat = at_info$mid_point

    if(toLog){

        if(DO_SPLIT){

            data_freq_valid = data_freq[isOther == FALSE, ]
            data_freq_valid[, x_num := as.numeric(x)]
            x_all = data_freq_valid$x_num
            exp_value = ceiling(exp(x_all))
            exp_value[x_all == -1] = 0

            if(maxFirst){

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
                    info_tilt = xaxis_biased(at = myat, max_line = max_line, labels = label_displayed)
                } else {
                    xaxis_labels(at = myat, labels = label_displayed)
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
                axis(1, at = myat, labels = NA, lwd.ticks = 1, lwd = 0, line = moreLine)

                # 2) The labels
                # Tilted labels not implemented for this axis
                if(delayLabelsTilted){
                    if(strwidth(paste0(exp_value_format, collapse = "  ")) / diff(get_x_lim()) > 0.9){
                        labels.tilted = TRUE
                    } else {
                        labels.tilted = FALSE
                    }

                }

                if(labels.tilted){
                    xaxis_biased(at = myat, labels = exp_value_format, yadj = 2, angle = 25)
                } else {
                    axis(1, at = myat, labels = exp_value_format, line = moreLine, lwd = 0)
                }


                # for extra ticking when info is ambiguous
                tck_location = par("usr")[3] - strheight("W")*81/100
                qui = which(exp_value < 10)
                if(length(qui) > 0){
                    # tick location
                    axis(2, at = tck_location - moreLine, pos = myat[qui], labels = NA, tcl = 0.15, xpd = TRUE)
                }

            }

            # Now the "other" ticks
            if(any(data_freq$isOther)){
                data_freq_other = data_freq[isOther == TRUE]

                # # we reconstruct the number -- A bit complicated... I know...
                # my_sign = gsub(" .+", "", data_freq_other$otherValue)
                # nber2show_raw = gsub(".+ ", "", data_freq_other$otherValue)
                #
                # nber2show = formatAxisValue(ceiling(exp(as.numeric(nber2show_raw))))
                #
                # qui_lower = my_sign == "<"
                # if(any(qui_lower)){
                #     nber2show[qui_lower] = formatAxisValue(ceiling(exp(as.numeric(nber2show_raw) - 1)))
                # }
                #
                # text2show = paste0(my_sign, " ", nber2show)
                #
                # # we get the right cex
                # w = data_freq_other[1, xright - xleft]
                # max_lab = text2show[which.max(strwidth(text2show))]
                #
                # minCex = 0.7
                # myCex = 1
                # while(myCex > minCex && strwidth(max_lab, cex = myCex) > w){
                #     myCex = myCex * 0.95
                # }
                #
                # axis(1, at = data_freq_other[, (xleft + xright)/2], labels = text2show, lwd = 0, line = -0.8, cex.axis = myCex)

                if(maxFirst){
                    text2show = rep("Other", nrow(data_freq_other))
                    my_font = 1
                } else {
                    text2show = rep(">", nrow(data_freq_other))
                    text2show[grepl("<", data_freq_other$otherValue)] = "<"
                    my_font = 2
                }

                myat = data_freq_other[, (xleft + xright)/2]

                if(maxFirst && labels.tilted){
                    xaxis_biased(at = myat, max_line = max_line, labels = text2show, angle = info_tilt$angle, cex = info_tilt$cex)
                } else {
                    axis(1, at = myat, labels = text2show, lwd = 0, line = -0.8, font = my_font)
                }


            }

        } else {
            #
            # formatting of the values
            #

            x_unik = at_info[x_nb %in% 1:maxBins, x]
            x_cases = length(x_unik)
            myat = at_info[x_nb %in% 1:maxBins, mid_point]
            exp_value = ceiling(exp(x_unik))
            exp_value[x_unik == -1] = 0

            if(is.unsorted(x_unik) || x_cases == 1 || any(diff(x_unik) != 1)){
                exp_value_right = ceiling(exp(x_unik + 1))

                # Formatting
                exp_value_format = substr(exp_value, 1, 7)
                exp_value_right_format = substr(exp_value_right, 1, 7)

                # finding the location
                location = xaxis_labels(at = myat, labels = paste0("[", exp_value_format, "; ", exp_value_right_format, "["), onlyParams = TRUE)

                # drawing
                for(i in 1:length(x_unik)){

                    value = substitute(group("[",list(x1, x2),")"), list(x1 = formatAxisValue(exp_value[i]), x2 = formatAxisValue(exp_value_right[i])))

                    axis(1, at = myat[i], lwd = 0, labels = value, cex = location$cex, line = 1 + location$line[i])
                }


            } else {
                # we draw the axes with nice display

                moreLine = (ADD_OTHER || ADD_OTHER_LEFT) * .25

                tck_location = par("usr")[3] - strheight("W")*81/100

                # Displaying the ticks "all at once" (including the last one)
                val = c(exp_value, ceiling(exp(tail(x_unik, 1) + 1)))
                exp_value_format = formatAxisValue(val)


                # tick location
                loc = (1:length(val)-1)*(moderator_cases+sep) - sep/2
                axis(1, at = loc, labels = exp_value_format, line = moreLine, lwd.ticks = 1, lwd = 0)

                for(i in 1:x_cases){
                    # tick location
                    loc = (i-1)*(moderator_cases+sep) - sep/2
                    # Ticks showing "strictly" inferior
                    if(x_unik[1] < 2){
                        axis(2, at = tck_location - moreLine, pos = loc, labels = NA, tcl = 0.15, xpd = TRUE)
                    }

                }

            }

            # We find the right cex to fit the "other" group
            unitary_width = par("pin")[1] / usr_width(xlim)
            too_large = function(text, the_cex, the_shift) strwidth(text, units = "in", cex = the_cex)/2 - the_shift*unitary_width > unitary_width*(1/2+sep/2*1.5)

            if(ADD_OTHER){

                # minCex = 0.75
                # myCex = 1
                # myShift = 0
                # maxShift = 1
                # while(myCex > minCex && too_large(otherText, myCex, myShift)){
                #     myCex = myCex * 0.95
                #     while(myShift < maxShift && too_large(otherText, myCex, myShift)){
                #         myShift = myShift + 0.1
                #     }
                #     if(too_large(otherText, myCex, myShift)) break
                #     myShift = 0
                #
                # }
                #
                #
                # axis(1, at = at_info[x_nb == maxBins + 1, mid_point] + maxShift, line = -1, labels = otherText, lwd = 0, cex.axis = myCex)

                axis(1, at = at_info[x_nb == maxBins + 1, mid_point], line = -0.8, labels = ">", lwd = 0, font = 2)

            }

            if(ADD_OTHER_LEFT){

                # minCex = 0.75
                # myCex = 1
                # myShift = 0
                # maxShift = 1
                # while(myCex > minCex && too_large(otherTextLeft, myCex, myShift)){
                #     myCex = myCex * 0.95
                #     while(myShift < maxShift && too_large(otherTextLeft, myCex, myShift)){
                #         myShift = myShift + 0.1
                #     }
                #     if(!too_large(otherTextLeft, myCex, myShift)) break
                #     myShift = 0
                #
                # }
                #
                # axis(1, at = at_info[x_nb == 0, mid_point] - myShift, line = -1, labels = otherTextLeft, lwd = 0, cex.axis = myCex)

                axis(1, at = at_info[x_nb == 0, mid_point], line = -0.8, labels = "<", lwd = 0, font = 2)

            }
        }

    } else if(numLabel){
        # moderator > 1 + split + numeric axis

        current_lim = get_x_lim()

        for(i in 1:moderator_cases){
            x_sub = data_freq[moderator == moderator_unik[i]]

            real_range = c(min(x_sub$xleft_real), max(x_sub$xright_real))
            x_show = pretty(real_range, 3)
            x_show = x_show[x_show >= real_range[1] & x_show <= real_range[2]]
            current_range = c(min(x_sub$xleft), max(x_sub$xright))
            x_show_current = to01(x_show, real_range) * diff(current_range) + min(current_range)
            axis(1, x_show_current, formatAxisValue(x_show))
        }

        # We add the bin information
        if(noSub){
            title(sub = substitute(paste("Bin size", phantom()==b), list(b = formatAxisValue(bin.size))), cex.sub = 0.9)
        }

        # now the Other
        if(any(data_freq$isOther)){
            data_freq_other = data_freq[isOther == TRUE]

            # we get the right cex
            w = data_freq_other[1, xright - xleft]
            max_lab = data_freq_other$otherValue[which.max(strwidth(data_freq_other$otherValue))]

            minCex = 0.7
            myCex = 1
            while(myCex > minCex && strwidth(max_lab, cex = myCex) > w){
                myCex = myCex * 0.95
            }

            axis(1, at = data_freq_other[, (xleft + xright)/2], labels = data_freq_other$otherValue, lwd = 0, line = -0.8, cex.axis = myCex)
        }


    } else if(numAxis){

        # We add the bin information
        if(noSub){
            title(sub = substitute(paste("Bin size", phantom()==b), list(b = formatAxisValue(bin.size))), cex.sub = 0.9)
        }

        myBox(1)

        if(ADD_OTHER){
            # the axis without last tick
            axis_at = axis(1, lwd = 0, labels = NA)
            axis(1, axis_at[1:(length(axis_at) - 1)])

            # the "other" text
            axis(1, at = at_info[x_nb == maxBins + 1, mid_point], line = -1, labels = otherText, lwd = 0)
        } else {
            axis(1)
        }

        if(ADD_OTHER_LEFT){
            # the "other" text
            axis(1, at = at_info[x_nb == 0, mid_point], line = -1, labels = otherTextLeft, lwd = 0)
        }

        # ticks at the right place
        if(moderator_cases > 1){
            # we add the rectangles to replicate a histogram
            # axis(1, at = data_freq[moderator_nb == 1, xleft], labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.45)
            rect_info = data_freq[, list(xleft = min(xleft), xright = max(xright), ytop = max(ytop)), by = x_nb]
            rect(xleft = rect_info$xleft, ybottom = 0, xright = rect_info$xright, ytop = rect_info$ytop, density = 0, lty = 2)
        }

    } else if(DO_SPLIT){
        # we need to display all xs

        # We add the bin information => specific case: max first + numeric data
        if(binned_data && noSub){
            title(sub = substitute(paste("Bin size", phantom()==b), list(b = formatAxisValue(bin.size))), cex.sub = 0.9)
        }

        data_freq[, mid_point := (xleft + xright) / 2]
        myLabels = data_freq$x
        myAt = data_freq$mid_point


        if(checkNotTilted){
            # If very short labels => we don't tilt them // allows to reintroduce xlab

            # axis_info = xaxis_labels(at = myAt, labels = myLabels, onlyParams = TRUE)
            # # if we reduce the labels => we tilt them
            # labels.tilted = axis_info$cex < 1 || any(axis_info$line != -1)
            if(sum(strwidth(myLabels, cex = 0.8)) < diff(par("usr")[1:2])){
                labels.tilted = FALSE
                if(noXlab){
                    title(xlab = x_name)
                }
            } else {
                labels.tilted = TRUE
            }
        }

        if(labels.tilted){
            xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, max_line = max_line)
        } else {
            xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method)
        }


    } else if(isNum){
        # we can have the "other" column both left and right

        x_unik = at_info[x_nb %in% 1:maxBins, x]
        myLabels = x_unik
        myAt = at_info[x_nb %in% 1:maxBins, mid_point]

        info_axis = NULL
        if(labels.tilted == FALSE && mean(diff(x_unik)) == 1){
            # This is a "normal" axis
            # everything number follows, this is fine

            # we still add the xlab we removed before (we could not anticipate the result)
            if(checkForTilting){
                if(noXlab) title(xlab = x_name)
            }

            axis(1, myAt, labels = myLabels)
        } else {
            if(checkForTilting){
                # If normal axis does not fit => tilt
                axis_info = xaxis_labels(at = myAt, labels = myLabels, onlyParams = TRUE)
                if(axis_info$failed){
                    labels.tilted = TRUE
                } else {
                    labels.tilted = FALSE
                    # we add the label only if the user didn't provide it before
                    if(noXlab) title(xlab = x_name)
                }
            }

            if(labels.tilted){
                info_axis = xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, max_line = max_line)
            } else {
                info_axis = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method, max_line = max_line)
            }
        }


        if(ADD_OTHER){
            # the "other" text

            if(is.null(info_axis)){
                axis(1, at = at_info[x_nb == maxBins + 1, mid_point] + strwidth("> "), line = -0.85, labels = otherText, lwd = 0, cex.axis = 1)
            } else if(labels.tilted){
                xaxis_biased(at = at_info[x_nb == maxBins + 1, mid_point], labels = otherText, angle = info_axis$angle, cex = info_axis$cex, trunc = trunc, trunc.method = trunc.method)
            } else {
                all_lines = sort(unique(info_axis$line))
                my_line = min(info_axis$line[info_axis$line != tail(info_axis$line, 1)])
                axis(1, at = at_info[x_nb == maxBins + 1, mid_point] + strwidth("> ", cex = info_axis$cex), line = my_line, labels = otherText, lwd = 0, cex.axis = info_axis$cex)
            }
        }

        if(ADD_OTHER_LEFT){
            # the "other" text on the left

            if(is.null(info_axis)){
                axis(1, at = at_info[x_nb == 0, mid_point] - strwidth("< "), line = -0.85, labels = otherTextLeft, lwd = 0)
            } else if(labels.tilted){
                xaxis_biased(at = at_info[x_nb == 0, mid_point], labels = otherTextLeft, angle = info_axis$angle, cex = info_axis$cex, trunc = trunc, trunc.method = trunc.method)
            } else {
                my_line = min(info_axis$line[info_axis$line != info_axis$line[1]])
                axis(1, at = at_info[x_nb == 0, mid_point] - strwidth("< ", cex = info_axis$cex), line = my_line, labels = otherTextLeft, lwd = 0, cex.axis = info_axis$cex)
            }
        }

    } else {

        # if(anyNA(at_info$x)){
        #     # in case we don't have numeric data: the other is made as a regular text to plot in the axis
        #     at_info$x[is.na(at_info$x)] = otherText
        #     maxBins = maxBins + 1
        # }
        if(ADD_OTHER){
            maxBins = maxBins + 1
            at_info$x[maxBins] = otherText
        }

        x_unik = at_info[x_nb %in% 1:maxBins, x]
        myLabels = x_unik
        myAt = at_info[x_nb %in% 1:maxBins, mid_point]

        if(checkForTilting){
            # If normal axis does not fit => tilt
            axis_info = xaxis_labels(at = myAt, labels = myLabels, onlyParams = TRUE)
            if(axis_info$failed){
                labels.tilted = TRUE
            } else {
                labels.tilted = FALSE
                # we add the label only if the user didn't provide it before
                if(noXlab) title(xlab = x_name)
            }
        }

        if(labels.tilted){
            info_axis = xaxis_biased(at = myAt, labels = myLabels, angle=labels.angle, cex = cex.axis, trunc = trunc, trunc.method = trunc.method, max_line = max_line)
        } else {
            info_axis = xaxis_labels(at = myAt, labels = myLabels, trunc = trunc, trunc.method = trunc.method)
        }

        # We also add ticks every 5/10 bins to help counting
        if(missing(tick_5)){
            tick_5 = max(at_info$x_nb) > 10
        }

        if(tick_5){
            qui = which(at_info$x_nb %% 5 == 0)
            qui = qui[qui != nrow(at_info)]
            if(length(at_info)){
                axis(3, at = (at_info[qui, mid_point] + at_info[qui+1, mid_point])/2, labels = NA, lwd = 0, lwd.ticks = 2, pos = 0, tck = -0.0075)
                qui_10 = intersect(qui, which(at_info$x_nb %% 10 == 0))
                if(length(qui_10)){
                    axis(1, at = (at_info[qui_10, mid_point] + at_info[qui_10+1, mid_point])/2, labels = NA, lwd = 0, lwd.ticks = 2, tck = -0.0075)
                }
            }
        }

    }


    if(yaxis.show){
        # we get the "nice" points display
        y_points = axis(2, lwd=0, col.axis = 0)

        # We don't go above 100%
        y_points = y_points[y_points <= 100]

        if(yaxis.num){
            # x1 = data_freq[x_nb == 1][1, ]
            x1 = data_freq[ytop > 0][1, ]
            y_labels = formatAxisValue(y_points / x1$ytop * x1$value)
        } else {
            y_labels = paste0(y_points, "%")
        }

        # we find the proper width
        if("ylab" %in% names(dots)){
            ylab = dots$ylab
        }

        if(nchar(ylab) == 0){
            myWidth = strwidth("7775%")
        } else {
            myWidth = strwidth("75%")
        }

        # We find the right cex
        minCex = 0.7
        myCex = 1
        y_labels_max = y_labels[which.max(strwidth(y_labels))]
        while(strwidth(y_labels_max, cex = myCex) > myWidth && myCex >= minCex){
            myCex = 0.95 * myCex
        }

        axis(2, at=y_points, labels = y_labels, las = 2, cex.axis = myCex)
    }

    # Legend if needed
    if(isLegend){
        #
        # finding the cex of the legend => depends on the longuest character
        #

        # legendFit("top", moderator_unik, bg="white", bty="o", box.col="white", fill=col[1:moderator_cases])

        legendFit("top", moderator_unik, fill=col[1:moderator_cases], title = mod.title, title_out = TRUE, trunc = info_legend$trunc)
    }

    # On top of the bars:

    if(onTop != "none"){

        qui = strwidth(top.value2display, units = "in", cex = top.cex) <= unitary_width_top*1.5 | nchar(top.value2display) <= 3

        # we don't display the 0s only
        qui = qui & info_top$value > 0

        # if(top.cex < 0.95 && onTop == "frac"){
        #     # we don't display the small numbers
        #     max_height = max(info_top$ytop)
        #     qui = qui & (info_top$ytop > max_height*0.05 | info_top$share_top >= 1)
        # }

        if(any(qui)){
            # text(data_freq[, (xleft+xright)/2][qui], data_freq$ytop[qui], label = top.value2display[qui], pos = 3, cex = top.cex, offset = 0.5 * top.cex * 0.9)
            text(info_top$x_mid[qui], info_top$ytop[qui], label = top.value2display[qui], pos = 3, cex = top.cex, offset = 0.5 * top.cex * 0.9)
        }

        if(DO_STACK && nrow(info_top_other) > 0){
            text(info_top_other$x_mid[qui], info_top_other$ytop[qui], label = top.value2display_other[qui], pos = 3, cex = top.cex_other, offset = 0.5 * top.cex_other * 0.9)
        }

    }

    return(invisible(data_freq))
}


#' Barplot with aggregate and moderator possibilities
#'
#' This functions draws a barplot in which the variables can have been aggregated beforehand.
#'
#' @inheritParams plot_distr
#'
#' @param fml A formula of the form: \code{var ~ agg} with \code{var} the variable, \code{agg} the variable over which to aggregate that will appear in the x-axis.
#' @param data A data.frame containing all the variables in the formula argument.
#' @param agg In the case \code{fml} is a vector, \code{agg} can be a vector of values over which to aggregate the main variable.
#' @param maxBins Defaults to 50. All other information that does not fit is put into the bin \dQuote{other}.
#' @param order Defaults to \code{FALSE}. Should the data be ordered w.r.t. frequency?
#' @param show0 Default to \code{FALSE}. Should the 0 be kept? By default, all the 0s are dropped.
#' @param cex.text The size of the text appearing on the top of the bins. Defaults to 0.7.
#' @param isDistribution Defaults to \code{FALSE}. It impacts the y-axis display. If it's a distribution, then percentages are shown on the y-axis.
#' @param yaxis.show Defaults to \code{FALSE}. Should the y-axis labels be displayed?
#' @param hgrid Default \code{TRUE}. Should the horizontal grid be displayed?
#' @param max_line Integer, default is 1. By defaults the labels of the x-axis can be displayed on several lines (from -1 to 1). This arguments says how far can the algorithm for the placement of the labels go downwards in the x-axis region.
#' @param showOther Default is \code{TRUE}. In the case the number of bins is lower than the number of cases, should the remaining obervations be displayed by a bar?
#' @param fun A function for the aggregation. Default is \code{mean}.
#' @param xlab The x-axis labels. By default it is the name of the aggregating variable.
#' @param ylab The y-axis labels. By default it is the name of the x variable combined with the function applied to it.
#' @param inCol Color of the interior of the bars. Default to blue.
#' @param outCol Color of the border of the bars. Defatult is black.
#' @param ... Other arguments to be passed to \code{barplot}.
#'
#' @author Laurent Berge
#'
#' @return
#' Invisibly returns the aggregated data.
#'
#' @examples
#'
#' plot_bar(Sepal.Length~Species, iris)
#'
#' plot_bar(Sepal.Length~Species, iris, fun = var, labels.tilted = TRUE)
#'
#'
#'
plot_bar = function(fml, data, agg, fun = mean, dict = getFplot_dict(), order=FALSE, maxBins=50, show0=TRUE, cex.text=0.7, isDistribution = FALSE, yaxis.show = TRUE, labels.tilted, trunc = 20, trunc.method = "auto", max_line, hgrid = TRUE, onTop = "nb", showOther = TRUE, inCol = "#386CB0", outCol = "white", xlab, ylab, ...){
    # this function formats a bit the data and sends it to myBarplot

    # Old params
    isLog = FALSE

    fml_in = fml

    # Controls
    check_arg(order, "singleLogical")
    check_arg(show0, "singleLogical")
    check_arg(isDistribution, "singleLogical")
    check_arg(labels.tilted, "singleLogical")
    check_arg(hgrid, "singleLogical")
    check_arg(showOther, "singleLogical")
    check_arg(maxBins, "singleIntegerGE1")
    check_arg(trunc, "singleIntegerGE1")
    check_arg(trunc.method, "singleCharacter")

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
    if("formula" %in% class(fml_in)){
        # Control of the formula

        if(missing(data) || !is.data.frame(data)){
            postfix = ifelse(!is.data.frame(data), paste0(" Currently it is of class ", enumerate_items(class(data))), "")

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
    x_name = getNames(x_name, dict)
    agg_name = getNames(agg_name, dict)

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

    myBarplot(x = res, order=order, maxBins=maxBins, show0=show0, cex.text=cex.text, isLog=isLog, isDistribution = isDistribution, yaxis.show = yaxis.show, niceLabels = TRUE, labels.tilted=labels.tilted, trunc = trunc, trunc.method = trunc.method, max_line=max_line, hgrid = hgrid, onTop = onTop, showOther = showOther, inCol = inCol, outCol = outCol, xlab = xlab, ylab = ylab, ...)

    invisible(base_agg)
}







#' Display means conditionnally on some other values
#'
#' The typical use of this function is to represents trends of average along some categorical variable.
#'
#' @inheritParams plot_distr
#'
#' @param fml A formula of the type \code{variable ~ time | moderator}. Note that the moderator is optional. Can also be a vector representing the elements of the variable If a formula is provided, then you must add the argument \sQuote{data}.
#' @param data Data frame containing the variables of the formula. Used only if the argument \sQuote{fml} is a formula.
#' @param time Only if argument \sQuote{fml} is a vector. It should be the vector of \sQuote{time} identifiers to average over.
#' @param moderator Only if argument \sQuote{fml} is a vector. It should be a vector of conditional values to average over. This is an optional parameter.
#' @param fun Function to apply when aggregating the values on the time variable. Default is \code{mean}.
#' @param mod.select Which moderators to select. By default the top 5 moderators in terms of frequency (or in terms of the value of fun in case of identical frequencies) are displayed. If provided, it must be a vector of moderator values whose length cannot be greater than 10. Alternatively, you can put an integer between 1 and 10.
#' @param smoothing_window Default is 0. The number of time periods to average over. Note that if it is provided the new value for each period is the average of the current period and the \code{smoothing_window} time periods before and after.
#' @param col The colors. Either a vector or a keyword (\dQuote{Set1} or \dQuote{paired}). By default those are the \dQuote{Set1} colors colorBrewer. This argument is used only if there is a moderator.
#' @param lty The line types, in the case there are more than one moderator. By default it is equal to 1 (ie no difference between moderators).
#' @param pch The form types of the points, in the case there are more than one moderator. By default it is equal to \8code{c(19, 17, 15, 8, 5, 4, 3, 1)}.
#' @param pt.cex Default to 2. The \code{cex} of the points.
#' @param lwd Default to 2. The width of the lines.
#' @param legend_options A list containing additional parameters for the function \code{\link[graphics]{legend}} -- only concerns the moderator. Not that you can set the additionnal arguments \code{trunc} and \code{trunc.method} which relates to the number of characters to show and the truncation method. By default the algorithm truncates automatically when needed.
#' @param ... Other arguments to be passed to the function \code{plot}.
#'
#' @author Laurent Berge
#'
#'
#' @examples
#'
#' df = iris
#' df$period = sample(1:4, 150, TRUE)
#'
#' plot_lines(Petal.Length ~ period|Species, df)
#'
#' plot_lines(Petal.Length ~ Species, df)
#'
#'
plot_lines = function(fml, data, time, moderator, mod.select, smoothing_window = 0, fun, col = "set1", lty = 1, pch = c(19, 17, 15, 8, 5, 4, 3, 1),  legend_options = list(), pt.cex=2, lwd=2, dict = getFplot_dict(), mod.title, ...){
    # This functions plots the means of x wrt the id
    # we can also add a moderator

    # DT VARS
    moderator_cases = xleft = xright = ybottom = ytop = value = NULL

    # old params
    style = "line"
    outCol = "black"
    # style = match.arg(style)

    mc = match.call()

    fml_in = fml

    check_arg(smoothing_window, "singleIntegerGE0")

    control_variable(dict, "nullCharacterVectorNoNA")

    # The color
    if(length(col) == 1){
        if(tolower(col) == "set1"){
            col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
        } else if(tolower(col) == "paired"){
            col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00")
        }
    }

    #
    # Extracting the information
    #

    moderator_name = ""
    if("formula" %in% class(fml_in)){
        # Control of the formula

        if(missing(data) || !is.data.frame(data)){
            postfix = ifelse(!is.data.frame(data), paste0(" Currently it is of class ", enumerate_items(class(data))), "")

            stop("If you provide a formula, a data.frame must be given in the argument 'data'.", postfix)
        }

        vars = all.vars(fml_in)
        if(any(!vars %in% names(data))){
            stop("The variable", enumerate_items(setdiff(vars, names(data)), "s.is")," not in the data set (", deparse(mc$data), ").")
        }

        # Creation of x and the condition
        if(!length(fml_in) == 3){
            stop("The formula must be of the type 'var ~ time' or 'var ~ time | moderator'.")
        }

        fml = extract_pipe(fml_in)$fml
        pipe = extract_pipe(fml_in)$pipe

        x = eval(fml[[2]], data)
        time = eval(fml[[3]], data)
        moderator = eval(pipe, data)

        if(is.null(moderator)){
            moderator = rep(1, length(x))
        } else {
            moderator_name = gsub("^.*\\| *", "", deparse(fml_in[[3]]))
        }

        # other info
        if(length(x) == 1){
            # this is a shortcut to say that we want to display the frequency
            fun = sum
            x_name = "Frequency"
        } else {
            x_name = deparse(fml[[2]])
        }

        time_name = deparse(fml[[3]])

    } else {
        if(missing(time)){
            stop("You must provide the argument 'time'.")
        } else {
            x = fml_in

            if(length(x) != length(time)){
                stop("The arguments 'x' and 'time' must be of the same length.")
            }

            if(!missnull(moderator)){
                if(length(x) != length(moderator)){
                    stop("If provided, the argument 'moderator' must be of the same length of 'x'.")
                }
                moderator_name = deparse(substitute(moderator))
            } else {
                moderator = rep(1, length(x))
            }

            # other info
            x_name = deparse(substitute(fml_in))
            time_name = deparse(substitute(time))
        }
    }

    # Naming
    x_name = getNames(x_name, dict)
    time_name = getNames(time_name, dict)
    moderator_name = getNames(moderator_name, dict)

    n_moderator = length(unique(moderator))
    if(is.factor(moderator)){
        moderator_unik = levels(moderator[drop = TRUE])
    } else {
        moderator_unik = sunique(as.character(moderator))
    }

    # Dropping NAs
    quiNA_x = is.na(x)
    quiNA_time = is.na(time)
    quiNA_mod = is.na(moderator)
    quiNA = quiNA_x | quiNA_time | quiNA_mod
    if(any(quiNA)){
        nb_na = c(sum(quiNA_x), sum(quiNA_mod), sum(quiNA_time))
        msg_na = paste0(c("x: ", "moderator: ", "time: "), nb_na)
        message("NOTE: ", sum(quiNA), " observations with NAs (", enumerate_items(msg_na[nb_na>0], "s.is"), ")")
        x = x[!quiNA]
        moderator = moderator[!quiNA]
        time = time[!quiNA]
    }

    # We first check the function is valid
    if(missing(fun)){
        fun = mean
    } else {
        if(!is.function(fun)){
            stop("Argument 'fun' must be a function. A the moment its class is ", class(fun)[1], ".")
        }

        test = try(fun(head(x, 5)), silent = TRUE)
        if("try-error" %in% class(test)){
            stop("Evaluation of the function '", deparse_long(mc$fun), "' leads to an error\n", test)
        } else if(length(test) > 1){
            stop("The function in argument 'fun' MUST be an aggregating function: that is returning something of length 1. This is not the case currently.")
        }

    }

    if(n_moderator == 1){
        isLegend = FALSE
    } else {
        isLegend = TRUE
    }

    # mod.select
    if(n_moderator > 1){

        n_select = NA
        do_recreate = FALSE
        if(missing(mod.select)){
            if(n_moderator <= 8){
                # FINE!
            } else {
                # We select the top 5
                n_select = 5
            }
        } else if(length(mod.select) > 10){
            stop("The argument 'mod.select' (currently of length ", length(mod.select), ") cannot be of a length greater than 10.")
        } else if(length(mod.select) == 1 && is.numeric(mod.select) && mod.select %% 1 == 0){
            # Possibly a number
            if(mod.select %in% 1:10){
                n_select = mod.select
            } else {
                stop("Argument 'mod.select' does not have a valid value: if it is an integer, its value must lie between 1 and 10.")
            }
        } else {
            # mod.select must contain moderator values

            # 1) checking the problems
            mod_pblm = setdiff(mod.select, moderator_unik)
            if(length(mod_pblm) > 0){

                if(length(mod_pblm) != length(mod.select) && any(quiNA)){
                    # means some are OK
                    suggestion = paste0(" Maybe because ", ifsingle(mod_pblm, "it has", "they have"), " only NA values?")
                } else {
                    suggestion = " It must consist of a vector of moderator values only."
                }

                stop("In the argument 'mod.select', the value", enumerate_items(mod_pblm, "s.is"), " not in the moderator variable.", suggestion)
            }

            # 2) selection
            do_recreate = TRUE

        }

        # AUTOMATIC selection
        if(!is.na(n_select) && n_moderator > n_select){

            # We proceed with the "automatic" selection

            info_mod = data.table(moderator = moderator)
            info_mod = info_mod[, list(value = .N), by = moderator]
            if(var(info_mod$value) == 0){
                # same freq for all
                info_mod = data.table(x = x, moderator = moderator)
                info_mod = info_mod[, list(value = fun(x)), by = moderator]
                info_method = paste0("the max of ", deparse(mc$fun)[1], ".")
            } else {
                info_method = "frequency."
            }

            # We inform on the method for the choice
            message(ifelse(n_select == 1, "The moderator was", paste0("The ", n_select, " moderators were")), " chosen based on ", info_method)

            info_mod = info_mod[order(-value)]
            mod.select = info_mod[1:n_select, moderator]

            do_recreate = TRUE

            if(n_select == 1){
                isLegend = FALSE
            }

        }

        if(do_recreate){
            # We recreate the variables

            qui_select = which(moderator %in% mod.select)
            # we recreate all the values
            x = x[qui_select]
            moderator = moderator[qui_select]
            time = time[qui_select]

            # Re-Dealing with the moderator
            if(is.factor(moderator)){
                moderator_unik = levels(moderator[drop = TRUE])
            } else {
                moderator_unik = sunique(moderator)
            }

            n_moderator = length(moderator_unik)
        }

    }

    #
    # Aggregation
    #

    quoi = data.table(x=x, time=time, moderator=moderator)
    base_agg = quoi[!is.na(x), list(x = fun(x)), by = list(time, moderator)]

    #
    # Preparation
    #

    dots = list(...)

    # ylim
    if(!is.null(dots$ylim)){
        ylim = dots$ylim
    } else if(style == "line"){
        ylim = range(base_agg[!is.na(moderator), x])
    } else if(style == "bar"){
        rx = range(base_agg[!is.na(moderator), x])
        ylim = c(min(0, rx[1]), max(0, rx[2]))
    }

    ymax = Inf
    if(isLegend){
        ymax = ylim[2]

        # if(missing(mod.title)){
        #     if(is.numeric(moderator) || is.logical(moderator)){
        #         mod.title = moderator_name
        #     } else {
        #         mod.title = NULL
        #     }
        # }
        if(missing(mod.title)){
            if(is.numeric(moderator) || is.logical(moderator)){
                mod.title = moderator_name
            } else {
                mod.title = NULL
            }
        } else if(isTRUE(mod.title)){
            mod.title = moderator_name
        } else if(isFALSE(mod.title)){
            mod.title = NULL
        }

        info_legend = legendFit(legend = moderator_unik, title = mod.title, plot = FALSE)
        legend_height = info_legend$total_height
        ylim[2] = ylim[2] + legend_height / par("pin")[2] * diff(ylim)
    }

    listDefault(dots, "ylim", ylim)
    listDefault(dots, "ylab", x_name)
    listDefault(dots, "xlab", time_name)
    dots$type = "n"

    dots$y = rep(1, length(unique(base_agg$time)))
    makeAxes = FALSE
    if(is.numeric(base_agg$time)){
        dots$x = sunique(base_agg$time)
    } else {
        # for character vectors, we need to recreate the xaxis
        if(is.null(dots$axes) || dots$axes){
            makeAxes = TRUE
        }
        dots$axes = FALSE
        if(is.factor(base_agg$time)){
            time_unik = levels(unique(base_agg$time)[, drop=TRUE])
            dict_time = 1:length(time_unik)
            names(dict_time) = time_unik
            dots$x = 1:length(time_unik)
            base_agg[, time := dict_time[as.character(time)]]
        } else {
            time_unik = sunique(base_agg$time)
            dots$x = quickUnclassFactor(time_unik)
            base_agg[, time := quickUnclassFactor(time)]
        }
    }



    if(style == "bar"){
        total_width = min(diff(sort(unique(base_agg$time))))
        bar_width = total_width / (n_moderator + 1)

        if(is.null(dots$xlim)){
            total_width = min(diff(sort(unique(base_agg$time))))
            dots$xlim = c(min(base_agg$time) - total_width * 0.5 + bar_width/2, max(base_agg$time) + total_width * 0.5 - bar_width/2)
        }
    } else {
        if(smoothing_window > 0){
            quoi = sort(dots$x)

            if(length(quoi) < 2*smoothing_window + 2){
                stop("To smooth with smoothing_window=", smoothing_window, " you need at least ", 2*smoothing_window + 2, " periods. Yet at the moment there is only ", length(quoi), " periods.")
            }

            dots$xlim = quoi[c(smoothing_window + 1, length(quoi) - smoothing_window)]
        }
    }

    do.call("plot", dots)
    dots$axes = NULL # not a graphical parameter afterwards

    if(style == "bar"){
        hgrid(ymax = ymax)
    } else {
        hgrid(ymax = ymax)
        vgrid(ymax = ymax)
        # grid(col = "darkgray")
    }

    if(makeAxes){
        box()
        axis(2)
        axis(1, dots$x, time_unik)
    }

    #
    # The plot_line
    #

    all_col = col[1 + (0:(n_moderator-1))%%length(col)]
    all_lty = lty[1 + (0:(n_moderator-1))%%length(lty)]
    all_pch = pch[1 + (0:(n_moderator-1))%%length(pch)]

    if(style == "line"){
        for(i in 1:n_moderator){
            dots$col = all_col[i]
            dots$lty = all_lty[i]
            dots$pch = all_pch[i]
            dots$cex = pt.cex
            dots$add = TRUE
            dots$smoothing_window = smoothing_window
            dots$x = base_agg[moderator == moderator_unik[i], time]
            dots$y = base_agg[moderator == moderator_unik[i], x]

            do.call("plot_line", dots)
        }
    } else if(style == "bar"){
        # Style is barplot
        base_agg[, moderator_cases := quickUnclassFactor(moderator)]

        base_agg[, xleft := time - total_width/2 + bar_width/2 + bar_width*(moderator_cases - 1)]
        base_agg[, xright := xleft + bar_width]
        base_agg[, ybottom := pmin(0, x)]
        base_agg[, ytop := pmax(0, x)]

        # the call to rect
        all_outcol = outCol[1 + (0:(n_moderator-1))%%length(outCol)]

        dots$xleft = base_agg$xleft
        dots$ybottom = base_agg$ybottom
        dots$xright = base_agg$xright
        dots$ytop = base_agg$ytop

        if(n_moderator > 1){
            dots$col = all_col[base_agg$moderator_cases]
            dots$border = all_outcol[base_agg$moderator_cases]
        } else {
            dots$col = col
            dots$border = outCol
        }


        dots = dots[names(dots) %in% names(formals(rect))]
        do.call("rect", dots)
    }


    #
    # Legend
    #

    if(n_moderator > 1 && isLegend){
        if(!is.list(legend_options)){
            stop("Argument'legend_options' must be a list.")
        }

        # mandatory
        legend_options$legend = moderator_unik
        if(style == "line"){
            legend_options$col = all_col
            legend_options$lty = all_lty
            legend_options$pch = all_pch
            legend_options$pt.cex = pmin(pt.cex, 1.5)
            legend_options$lwd = lwd
        } else if(style == "bar"){
            legend_options$fill = all_col
        }

        legend_options$title = mod.title
        legend_options$trunc = info_legend$trunc


        # options
        listDefault(legend_options, "x", "top")

        # the call
        do.call("legendFit", legend_options)
    }

    invisible(base_agg)
}


#' Boxplots with possibly moderators
#'
#' This function allows to draw a boxplot, with possibly separating different moderators.
#'
#' @inheritParams plot_distr
#' @inheritParams plot_bar
#'
#' @param fml A formula of the type: \code{var ~ case | moderator}. Note that if a formula is provided then the argument \sQuote{data} must be provided.
#' @param data A data.frame/data.table containing the relevant information.
#' @param case When argument fml is a vector, this argument can receive a vector of cases.
#' @param moderator When argument fml is a vector, this argument can receive a vector of moderators.
#' @param inCol A vector of colors that will be used for within the boxes.
#' @param outCol The color of the outer box. Default is black.
#' @param pch The patch of the outliers. Default is 18.
#' @param addLegend Default is \code{TRUE}. Should a legend be added at the top of the graph is there is more than one moderator?
#' @param lwd The width of the lines making the boxes. Default is 2.
#' @param outlier Default is \code{TRUE}. Should the outliers be displayed?
#' @param dict_case A named character vector. If provided, it changes the values of the variable \sQuote{case} to the ones contained in the vector \code{dict_case}. Example: I want to change my variable named "a" to "Australia" and "b" to "Brazil", then I used \code{dict=c(a="Australia",b="Brazil")}.
#' @param dict_moderator A named character vector. If provided, it changes the values of the variable \sQuote{moderator} to the ones contained in the vector \code{dict_moderator}. Example: I want to change my variable named "a" to "Australia" and "b" to "Brazil", then I used \code{dict=c(a="Australia",b="Brazil")}.
#' @param order_case Character vector. This element is used if the user wants the \sQuote{case} values to be ordered in a certain way. This should be a regular expression (see \code{\link[base]{regex}} help for more info). There can be more than one regular expression. The variables satisfying the first regular expression will be placed first, then the order follows the sequence of regular expressions.
#' @param order_moderator Character vector. This element is used if the user wants the \sQuote{moderator} values to be ordered in a certain way. This should be a regular expression (see \code{\link[base]{regex}} help for more info). There can be more than one regular expression. The variables satisfying the first regular expression will be placed first, then the order follows the sequence of regular expressions.
#' @param addMean Whether to add the average for each boxplot. Default is true.
#' @param mean.col The color of the mean. Default is darkred.
#' @param mean.cex The cex of the mean, default is 2.
#' @param mean.pch The patch of the mean, default is 18.
#' @param max_line Option for the x-axis, how far should the labels go. Default is 1 for normal labels, 2 for tilted labels.
#' @param density The density of lines within the boxes. By default it is equal to -1, which means the boxes are filled with color.
#' @param lty The type of lines for the border of the boxes. Default is 1 (solid line).
#' @param ... Other parameters to be passed to \code{plot}.
#'
#' @author Laurent Berge
#'
#' @return
#' Invisibly returns the coordinates of the x-axis.
#'
#' @examples
#'
#' m = iris
#' m$period = sample(1:4, 150, TRUE)
#'
#' plot_box(Petal.Length ~ period|Species, m)
#'
plot_box = function(fml, data, case, moderator, inCol, outCol = "black", density = -1, lty = 1, pch = 18, addLegend = TRUE,  legend_options = list(), lwd=2, outlier, dict_case, dict_moderator, order_case, order_moderator, addMean, mean.col = "darkred", mean.pch = 18, mean.cex = 2, mod.title, labels.tilted, trunc = 20, trunc.method = "auto", max_line, dict = getFplot_dict(), ...){

    # DT VARS USED
    case_nb = moderator_nb = span = q3 = q1 = min_whisker = y_min = max_whisker = y_max = NULL

    fml_in = fml

    mc = match.call()

    # Controls

    check_arg(addLegend, "singleLogical")
    check_arg(outlier, "singleLogical")
    check_arg(order_case, "singleLogical")
    check_arg(order_moderator, "singleLogical")
    check_arg(addMean, "singleLogical")
    check_arg(mod.title, "singleCharacter")
    check_arg(labels.tilted, "singleLogical")
    check_arg(trunc, "singleIntegerGE5")

    #
    # Extracting the information
    #

    moderator_name = ""
    if("formula" %in% class(fml_in)){

        if(missing(data) || !is.data.frame(data)){
            postfix = ifelse(!is.data.frame(data), paste0(" Currently it is of class ", enumerate_items(class(data)), "."), "")

            stop("If you provide a formula, a data.frame must be given in the argument 'data'.", postfix)
        }

        vars = all.vars(fml_in)
        if(any(!vars %in% names(data))){
            stop("The variable", enumerate_items(setdiff(vars, names(data)), "s.is")," not in the data set (", deparse_long(mc$data), ").")
        }

        # Creation of x and the condition
        if(!length(fml_in) == 3){
            stop("The formula must be of the type 'var ~ case' or 'var ~ case | moderator'.")
        }

        fml = extract_pipe(fml_in)$fml
        pipe = extract_pipe(fml_in)$pipe

        x = eval(fml[[2]], data)
        case = eval(fml[[3]], data)
        moderator = eval(pipe, data)

        if(is.null(moderator)){
            moderator = rep(1, length(x))
        } else {
            moderator_name = gsub("^.*\\| *", "", deparse(fml_in[[3]]))
        }

        # other info
        x_name = deparse(fml[[2]])
        case_name = deparse(fml[[3]])

    } else {
        if(missing(case)){
            stop("You must provide the argument 'case'.")
        } else {
            x = fml_in

            if(length(x) != length(case)){
                stop("The arguments 'x' and 'case' must be of the same length.")
            }

            if(!missing(moderator) && !is.null(moderator)){
                if(length(x) != length(moderator)){
                    stop("If provided, the argument 'moderator' must be of the same length of 'x'.")
                }
                moderator_name = clean_name(deparse(substitute(moderator)))
            } else {
                moderator = rep(1, length(x))
            }

            # other info
            x_name = clean_name(deparse(substitute(fml)))
            case_name = clean_name(deparse(substitute(case)))
        }
    }

    # Dropping NAs
    quiNA_x = is.na(x)
    quiNA_mod = is.na(moderator)
    quiNA_case = is.na(case)
    quiNA = quiNA_x | quiNA_mod | quiNA_case
    if(any(quiNA)){
        nb_na = c(sum(quiNA_x), sum(quiNA_mod), sum(quiNA_case))
        msg_na = paste0(c("x: ", "moderator: ", "case: "), nb_na)
        message("NOTE: ", sum(quiNA), " observations with NAs (", enumerate_items(msg_na[nb_na>0]), ").")
        x = x[!quiNA]
        moderator = moderator[!quiNA]
        case = case[!quiNA]
    }

    # Renaming: dict
    x_name = getNames(x_name, dict)
    case_name = getNames(case_name, dict)
    moderator_name = getNames(moderator_name, dict)


    #
    # Aggregation
    #

    mod_num_logical = is.logical(moderator) || is.numeric(moderator) # used later
    moderator = as.character(moderator)

    CASE_FACTOR = FALSE
    if(is.factor(case)){
        CASE_FACTOR = TRUE
        case_unik = levels(case[, drop = TRUE])
        case = unclass(case[, drop = TRUE])
    }


    quoi = data.table(x=as.numeric(x), case=case, moderator=moderator)

    delayAddMean = FALSE
    if(missnull(addMean)){
        addMean = TRUE
        delayAddMean = TRUE
    }

    if(addMean){
        base_agg = quoi[, list(y_min = min(x), q1 = quantile(x, 0.25), m = median(x), q3 = quantile(x, 0.75), y_max = max(x), avg = mean(x)), keyby = list(case, moderator)]
    } else {
        base_agg = quoi[, list(y_min = min(x), q1 = quantile(x, 0.25), m = median(x), q3 = quantile(x, 0.75), y_max = max(x)), keyby = list(case, moderator)]
    }


    #
    # the coordinates
    #

    # putting case to number and formatting it
    if(!missnull(order_case)){
        case_unik = sunique(case)
        for(var2order in rev(order_case)){
            who = grepl(var2order, case_unik)
            case_unik = c(case_unik[who], case_unik[!who])
        }
        base_agg[, case_nb := dict2number(case_unik, case)]
    } else {
        base_agg[, case_nb := quickUnclassFactor(case)]
        if(CASE_FACTOR == FALSE){
            what = unique(base_agg[, list(case, case_nb)])
            what = what[order(case_nb)]
            case_unik = what$case
        }
    }

    aliasCase = case_unik
    if(!missing(dict_case) && !is.null(dict_case)){
        if(!is.character(dict_case) || is.null(names(dict_case))) stop("the arg. 'dict_case' must be a named character vector.")

        qui = which(case_unik %in% names(dict_case))
        aliasCase[qui] = dict_case[case_unik[qui]]
    }

    # putting moderator to number and formatting it
    if(!missnull(order_moderator)){
        moderator_unik = sunique(moderator)
        for(var2order in rev(order_moderator)){
            who = grepl(var2order, moderator_unik)
            moderator_unik = c(moderator_unik[who], moderator_unik[!who])
        }
        base_agg[, moderator_nb := dict2number(moderator_unik, moderator)]
    } else {
        base_agg[, moderator_nb := quickUnclassFactor(moderator)]
        moderator_unik = sunique(moderator)
    }

    aliasModerator = moderator_unik
    if(!missing(dict_moderator) && !is.null(dict_moderator)){
        if(!is.character(dict_moderator)|| is.null(names(dict_moderator))) stop("The argument 'dict_moderator' must be a named character vector.")

        qui = which(moderator_unik %in% names(dict_moderator))
        aliasModerator[qui] = dict_moderator[moderator_unik[qui]]
    }

    n_case = length(case_unik)
    n_moderator = length(moderator_unik)

    # The colors
    if(missnull(inCol)){
        if(n_moderator == 1){
            inCol = "#1F78B4"
        } else {
            inCol = c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00")
        }
    }

    # separation between the different cases:
    sep = 0.30 + 0.15 * (n_moderator-1)

    base_agg[, x := (case_nb-1)*(n_moderator+sep) + moderator_nb]

    # Information on the length of the whiskers
    base_agg[, span := 1.5 * (q3 - q1)]
    base_agg[, min_whisker := pmax(y_min, q1 - span)]
    base_agg[, max_whisker := pmin(y_max, q3 + span)]

    # the legend
    isLegend = FALSE
    if(addLegend && n_moderator > 1){
        isLegend = TRUE
    }

    # mod.title
    if(isLegend){
        if(missing(mod.title)){
            if(mod_num_logical){
                mod.title = moderator_name
            } else {
                mod.title = NULL
            }
        } else if(isTRUE(mod.title)){
            mod.title = moderator_name
        } else if(isFALSE(mod.title)){
            mod.title = NULL
        }
    }


    #
    # Preparation
    #

    dots = list(...)

    # default for outlier
    if(missnull(outlier)){
        outlier = FALSE
        if(diff(range(x)) < 2 * diff(range(c(base_agg$min_whisker, base_agg$max_whisker)))){
            outlier = TRUE
        }
    }

    # default for addMean
    if(delayAddMean){
        addMean = !outlier
    }

    # ylim

    if(outlier){
        ylim = range(quoi$x)
    } else {
        ylim = c(min(base_agg$min_whisker), max(base_agg$max_whisker))
    }

    if(isLegend){
        info_legend = legendFit(legend = aliasModerator, plot = FALSE, title = mod.title)
        # hauteur_caractere = strheight("W", "in", cex = info_legend$cex)
        # ylim[2] = ylim[2] + 2.5*hauteur_caractere / par("pin")[2] * diff(ylim)
        total_height = info_legend$total_height
        ylim[2] = ylim[2] + total_height / par("pin")[2] * diff(ylim)
    }

    if(missnull(labels.tilted)){
        if(sum(nchar(aliasCase)) * strwidth("w", "in") > 2 * par("pin")[1]){
            labels.tilted = TRUE
            case_name = ""
        } else {
            labels.tilted = FALSE
        }
    } else if(labels.tilted){
        case_name = ""
    }

    listDefault(dots, "ylim", ylim)
    listDefault(dots, "ylab", x_name)
    listDefault(dots, "xlab", case_name)
    dots$type = "n"

    isAxes = TRUE
    if(!is.null(dots$axes)){
        isAxes = dots$axes
    }
    dots$axes = FALSE

    dots$x = dots$y = 1

    # xlim
    dots$xlim = range(base_agg$x) + c(-0.5, 0.5)

    do.call("plot", dots)

    if(isAxes){
        box()
        axis(2)

        at_labels = 1 + (n_moderator-1)/2 + (0:(n_case-1))*(n_moderator+sep)
        # axis(1, at = at_labels, labels = aliasCase, lwd = 0)

        if(labels.tilted){
            if(missnull(max_line)) max_line = 2
            xaxis_biased(at = at_labels, labels = aliasCase, trunc = trunc, trunc.method = trunc.method, max_line=max_line)
        } else {
            if(missnull(max_line)) max_line = 1
            xaxis_labels(at = at_labels, labels = aliasCase, trunc = trunc, trunc.method = trunc.method, max_line=max_line)
        }

    }

    hgrid()

    #
    # The plot_line
    #

    if(n_moderator == 1){
        all_inCol = inCol[1 + (0:(n_case-1))%%length(inCol)]
        all_outCol = outCol[1 + (0:(n_case-1))%%length(outCol)]
        all_densities = density[1 + (0:(n_case-1))%%length(density)]
    } else {
        all_inCol = inCol[1 + (0:(n_moderator-1))%%length(inCol)]
        all_outCol = outCol[1 + (0:(n_moderator-1))%%length(outCol)]
        all_densities = density[1 + (0:(n_moderator-1))%%length(density)]
    }


    for(i in 1:n_moderator){
        quoi = base_agg[moderator == moderator_unik[i]]

        if(n_moderator == 1){
            box_single(x = quoi$x, quoi$y_min, quoi$q1, quoi$m, quoi$q3, quoi$y_max, width = 1, inCol = all_inCol, lwd = lwd, outCol = all_outCol, density = all_densities)
        } else {
            box_single(x = quoi$x, quoi$y_min, quoi$q1, quoi$m, quoi$q3, quoi$y_max, width = 1, inCol = all_inCol[i], lwd = lwd, outCol = all_outCol[i], density = all_densities[i])
        }


        # now the outliers
        if(outlier){
            for(j in 1:n_case){
                quoi_case = quoi[case == case_unik[j]]
                y = x[moderator == moderator_unik[i] & case == case_unik[j]]
                qui_outlier = y > quoi_case$max_whisker | y <  quoi_case$min_whisker
                if(any(qui_outlier)){
                    points(rep(quoi_case$x, sum(qui_outlier)), y[qui_outlier], pch=pch)
                }
            }
        }

        if(addMean){
            quoi = base_agg[moderator == moderator_unik[i]]
            points(quoi$x, quoi$avg, col = mean.col, pch = mean.pch, cex = mean.cex)
        }
    }

    #
    # Legend
    #

    if(isLegend){
        if(!is.list(legend_options)){
            stop("Argument'legend_options' must be a list.")
        }

        # mandatory
        legend_options$legend = aliasModerator
        legend_options$fill = all_inCol
        legend_options$title = mod.title

        # options
        listDefault(legend_options, "x", "top")

        # the call
        do.call("legendFit", legend_options)
    }

    invisible(1 + (n_moderator-1)/2 + (0:(n_case-1))*(n_moderator+sep))
}





