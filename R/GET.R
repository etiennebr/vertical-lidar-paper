# Functions adapted from the GET package 

# Return the data for the plot function, rather than plot
# (allows for custom plots)
my_plot_data_combined_global_envelope <- function (
  x, main, ylim = NULL, xlab, ylab, env.col = 1, color_outside = TRUE, 
  sign.col = "red", base_size = 12, labels = NULL, add = FALSE, 
  digits = 3, level = 1, ncol = 2 + 1 * (length(x) == 3), 
  nticks = 5, legend = TRUE, scale_r = NA, split = TRUE, ...) 
{
  stopifnot(requireNamespace("GET"))
  if (!(level %in% c(1, 2))) 
    stop("Unreasonable value for level.\n")
  if (missing("main")) {
    alt <- GET:::get_alternative(x[[1]])
    main <- GET:::env_main_default(attr(x, "level2_ge"), digits = digits, 
                             alternative = alt)
  }
  if (missing("xlab")) 
    if (is.expression(attr(attr(x, "level2_ge"), "xexp"))) 
      xlab <- substitute(i, list(i = attr(attr(x, "level2_ge"), 
                                          "xexp")))
    else xlab <- substitute(italic(i), list(i = attr(attr(x, 
                                                          "level2_ge"), "xexp")))
    if (missing("ylab")) 
      if (is.expression(attr(attr(x, "level2_ge"), "yexp"))) 
        ylab <- substitute(i, list(i = attr(attr(x, "level2_ge"), 
                                            "yexp")))
      else ylab <- substitute(italic(i), list(i = attr(attr(x, 
                                                            "level2_ge"), "yexp")))
      if (is.null(labels)) {
        if (!is.null(attr(x, "labels"))) 
          labels <- attr(x, "labels")
        else {
          if (!is.null(names(x))) 
            labels <- names(x)
          else {
            labels <- sapply(x, function(y) attr(y, "ylab"), 
                             simplify = TRUE)
            if (all(sapply(labels, FUN = identical, y = labels[[1]]))) 
              labels <- NULL
          }
        }
      }
      if (level == 1) {
        my_env_ggplot(x, base_size = base_size, main = main, ylim = ylim, 
                      xlab = xlab, ylab = ylab, max_ncols_of_plots = ncol, 
                      labels = labels, nticks = nticks, legend = legend, 
                      scale_r = scale_r, split = split, ...)
      }
      else {
        stop("TODO: replace plot.global_envelope")
        plot.global_envelope(attr(x, "level2_ge"), dotplot = TRUE, 
                             main = main, ylim = ylim, xlab = xlab, ylab = ylab, 
                             color_outside = color_outside, env.col = env.col, 
                             base_size = base_size, labels = labels, add = add, 
                             digits = digits, ...)
      }
}

my_env_ggplot <- function (x, base_size, main, ylim, xlab, ylab, max_ncols_of_plots = 2, 
                           labels = NULL, nticks = 5, curve_sets = NULL, x2 = NULL, 
                           legend = TRUE, color_outside = TRUE, sign.col = "red", scale_r = NA, split = TRUE) 
{
  if (!inherits(x, "list")) 
    x <- list(x)
  Nfunc <- length(x)
  if (!is.null(x2)) {
    if (!inherits(x2, "list")) 
      x2 <- list(x2)
    if (length(x) != length(x2)) {
      warning("Unsuitable x2. Setting it to NULL.\n")
      x2 <- NULL
    }
    else {
      for (i in 1:length(x)) {
        if (!all(x[[i]][["r"]] == x2[[i]][["r"]])) 
          stop("The two envelopes are for different r-values.\n")
        if (!all(x[[i]][["central"]] == x2[[i]][["central"]])) 
          warning("The two envelopes have different central functions!\n")
      }
    }
    rdata <- GET:::combined_global_envelope_rhelper(x2, nticks = nticks)
    x2 <- rdata$x_vec
  }
  rdata <- GET:::combined_global_envelope_rhelper(x, nticks = nticks)
  alt <- GET:::get_alternative(x[[1]])
  x <- rdata$x_vec
  linetype.values <- c("dashed", "solid")
  size.values <- c(0.2, 0.2)
  counter <- 0
  outliers <- NULL
  if (!is.null(curve_sets)) {
    if (inherits(curve_sets, "list")) 
      curve_sets <- GET:::combine_curve_sets(curve_sets, equalr = FALSE)
    funcs <- GET:::curve_set_funcs(curve_sets)
    for (j in 1:ncol(funcs)) {
      if (any(funcs[, j] < x[["lo"]] | funcs[, j] > x[["hi"]])) {
        outliers <- c(outliers, funcs[, j])
        counter <- counter + 1
      }
    }
  }
  if (Nfunc == 1 & is.null(rdata$r_values_newstart_id)) {
    if (rdata$retick_xaxis) 
      x[["r"]] <- 1:length(x[["r"]])
    if (is.null(x[["obs"]])) {
      df <- data.frame(r = x[["r"]], curves = x[["central"]], 
                       type = factor("Central function", levels = "Central function"), 
                       lower = x[["lo"]], upper = x[["hi"]], main = main)
      if (!is.null(x2)) {
        df$lower2 <- x2[["lo"]]
        df$upper2 <- x2[["hi"]]
      }
    }
    else {
      df <- data.frame(r = rep(x[["r"]], times = 2), curves = c(x[["obs"]], 
                                                                x[["central"]]), type = factor(rep(c("Data function", 
                                                                                                     "Central function"), each = length(x[["r"]])), 
                                                                                               levels = c("Central function", "Data function")), 
                       lower = rep(x[["lo"]], times = 2), upper = rep(x[["hi"]], 
                                                                      times = 2), main = main)
      if (!is.null(x2)) {
        df$lower2 <- rep(x2[["lo"]], times = 2)
        df$upper2 <- rep(x2[["hi"]], times = 2)
      }
    }
    p <- list(ribbon = df, x2 = is.null(x2), x_obs = is.null(x[["obs"]]))
  }
  else {
    if (Nfunc == 1) 
      warning("The r-values are non-increasing in the given object. Splitting to several plots.\n")
    n_of_plots <- as.integer(1 + length(rdata$r_values_newstart_id))
    ncols_of_plots <- min(n_of_plots, max_ncols_of_plots)
    nrows_of_plots <- ceiling(n_of_plots/ncols_of_plots)
    if (is.null(labels)) 
      labels <- paste(1:n_of_plots)
    if (length(labels) != n_of_plots) {
      if (length(labels) == 1) {
        labels <- paste(labels, " - ", 1:n_of_plots, 
                        sep = "")
        warning(paste("Consider giving labels as a vector of length ", 
                      n_of_plots, " containing the label for each test function/vector used.\n", 
                      sep = ""))
      }
      else {
        warning("The length of the vector labels is unreasonable. Setting labels to empty.\n")
        labels <- rep("", times = n_of_plots)
      }
    }
    tmp_indeces <- c(1, rdata$r_values_newstart_id, length(rdata$new_r_values) + 
                       1)
    func_labels <- NULL
    for (i in 1:(length(tmp_indeces) - 1)) {
      func_labels <- c(func_labels, rep(labels[i], times = tmp_indeces[i + 
                                                                         1] - tmp_indeces[i]))
    }
    if (is.null(x[["obs"]])) {
      df <- data.frame(r = x[["r"]], curves = x[["central"]], 
                       type = factor("Central function", levels = "Central function"), 
                       lower = x[["lo"]], upper = x[["hi"]], main = main, 
                       test_function = factor(func_labels, levels = labels))
      if (!is.null(x2)) {
        df$lower2 <- x2[["lo"]]
        df$upper2 <- x2[["hi"]]
      }
    }
    else {
      df <- data.frame(r = rep(x[["r"]], times = 2), curves = c(x[["obs"]], 
                                                                x[["central"]]), type = factor(rep(c("Data function", 
                                                                                                     "Central function"), each = length(x[["r"]])), 
                                                                                               levels = c("Central function", "Data function")), 
                       lower = rep(x[["lo"]], times = 2), upper = rep(x[["hi"]], 
                                                                      times = 2), main = main, test_function = factor(func_labels, 
                                                                                                                      levels = labels))
      if (!is.null(x2)) {
        df$lower2 <- rep(x2[["lo"]], times = 2)
        df$upper2 <- rep(x2[["hi"]], times = 2)
      }
    }
    p <- list(ribbon = df, x2 = is.null(x2), x_obs = is.null(x[["obs"]]))

    if (!is.null(outliers)) {
      outliers.df <- data.frame(r = rep(x[["r"]], times = counter), 
                                curves = outliers, id = rep(1:counter, each = length(x[["r"]])), 
                                test_function = factor(func_labels, levels = labels))
      p$outliers <- outliers.df
    }
  }
  if (!is.null(x[["obs"]])) {
    if (color_outside) {
      df.outside <- df[df$type == "Data function", ]
      df.outside <- df.outside[df.outside$curves < df.outside$lower | 
                                 df.outside$curves > df.outside$upper, ]
      p$df_outside <- df.outside
    }
  }
  p$main = main
  
  if (!is.na(scale_r)) {
    p$ribbon <- p$ribbon %>% 
      mutate(
        r = r / scale_r
      )
    
    p$df_outside <- p$df_outside %>% 
      mutate(
        r = r / scale_r
      ) 
  }
  
  # standardize name
  if (!"test_function" %in% names(p$ribbon)) {
    names(p$ribbon)[names(p$ribbon) == "main"] <- "test_function"
    names(p$df_outside)[names(p$df_outside) == "main"] <- "test_function"
  }
  
  if (isTRUE(split)) {
    p$ribbon <- p$ribbon %>% 
      split_labels(test_function) %>% 
      # contrasts don't have a what
      mutate(
        swap = is.na(what_b) & is.na(b),
        b = ifelse(swap, a, b),
        a = ifelse(swap, what_a, a),
        what_a = ifelse(swap, NA, what_a),
        what_b = ifelse(swap, NA, what_b)
      ) %>% 
      select(-swap) %>% 
      mutate(
        a = sort_num_factor(a),
        b = sort_num_factor(b)
      )
    
    
    p$df_outside <- p$df_outside %>% 
      split_labels(test_function) %>% 
      mutate(
        a = sort_num_factor(a),
        b = sort_num_factor(b)
      )
  }
  
  
  
  # add inside and lead point
  p$ribbon <- p$ribbon %>% 
    dplyr::group_by(type, test_function) %>% 
    dplyr::mutate(
      x_end = lead(r, default = NA),
      y_end = lead(curves, default = NA),
      n = dplyr::n(),
      inside = (curves > lower) & (curves < upper),
      inside_lead = lead(inside)
    )
  return(p)
}

fill_contrast_matrix <- function(x) {
  suppressWarnings(bind_rows(x, flip_contrast_matrix(x))) %>% 
    ungroup() %>% 
    mutate_at(c("a", "b"), sort_num_factor)
}

flip_contrast_matrix <- function(x) {
  #assumes that the central function is = 0
  mutate(x,
    # permute factors
    temp = a,
    a = b,
    b = temp,
    temp = what_a,
    what_a = what_b,
    what_b = temp,
    
    
    # permute envelope
    lower = -lower,
    upper = -upper,
    
    # permute curve
    curves = -curves,
    y_end = -y_end
  ) %>% 
    select(-temp)
}

# split test_function
split_labels <- function(x, col) tidyr::separate(x, {{col}}, c("what_a", "a", "what_b", "b"), sep = "[\\.-]", remove = FALSE)

sort_num_factor <- function(x, levels = stringr::str_sort(unique(x), numeric = TRUE)) factor(x, levels = levels)
