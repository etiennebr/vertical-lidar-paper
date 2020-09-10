# plotting functions for functional data

# colors
envelope_grey <- dark_grey
line_inside <- "grey20"
line_outside <- "red"

# sizes
line_inside <- 0.2
line_outside <- 0.6

# custom plot -----------------------------------------------------------------

my_plot_continuous <- function(x, scale_r, ...) {
  pd <- my_plot_data_combined_global_envelope(x, scale_r = scale_r, ...) 
  
  ribbon <- pd$ribbon %>% 
    filter(
      type == "Data function"
    ) 
  
  facet <- NULL
  if (length(unique(ribbon$b)) > 1) {
    facet <- list(facet_grid(~b))
  }
  
  p <- 
    ribbon %>% 
    ggplot(aes(x = r, y = curves, group = interaction(type, a, b))) + 
    geom_ribbon(aes(x = r, ymin = lower, ymax = upper), fill = envelope_grey, alpha = 1) +
    geom_segment(aes(xend = x_end, yend = y_end, size = ifelse(!inside & !inside_lead, "outside", "inside")), show.legend = FALSE, color = "grey20") +
    scale_color_manual(values = c("outside" = line_outside, "inside" = line_inside)) +
    scale_size_manual(values = c("outside" = line_outside, "inside" = line_inside)) +
    facet +
    xlab("Scaled Stand Height") +
    ylab("Scaled density difference") +
    coord_flip()
  
  return(p)
}

my_plot_contrasts <- function(x, scale_r, upper = !full, full = FALSE, diag = !full, add) {
  pd <- my_plot_data_combined_global_envelope(x, scale_r = scale_r) 
  
  ribbon <- pd$ribbon %>% 
    filter(
      type == "Data function"
    ) 
  
  if (full) {
    ribbon <- ribbon %>% 
      fill_contrast_matrix() %>% 
      mutate(
        a = sort_num_factor(a),
        b = sort_num_factor(b)
      ) 
  }
  
  p <- 
    ribbon %>% 
    ggplot(aes(x = r, y = curves, group = interaction(type, a, b))) + 
    geom_ribbon(aes(x = r, ymin = lower, ymax = upper), fill = envelope_grey, alpha = 1) +
    #geom_line(size = 0.3) +
    #geom_segment(aes(xend = x_end, yend = y_end, color = ifelse(!inside & !inside_lead, "outside", "inside")), show.legend = FALSE, size = 1) +
    geom_segment(aes(xend = x_end, yend = y_end, size = ifelse(!inside & !inside_lead, "outside", "inside")), show.legend = FALSE, color = "grey20") +
    scale_color_manual(values = c("outside" = line_outside, "inside" = line_inside)) +
    scale_size_manual(values = c("outside" = line_outside, "inside" = line_inside)) +
    #geom_point(aes(color = inside), size = 0.5) +
    facet_grid(b~a) +
    xlab("Stand Height") +
    ylab("Estimates") +
    coord_flip()
  
  if (!missing(add)) {
    p <- add_diag_panels(p, add)
  }
  
  if (upper) {
    p <- remove_upper_panels(p)
  }
  
  if (!diag) {
    p <- remove_diag_panels(p)
  }
  
  return(p)
}

remove_diag_panels <- function(x) {
  # Remove empty panels
  # Adapted from: https://en.it1352.com/article/d0a0d77cc54e4a2ea01ec2085831c390.html
  g <- ggplotGrob(x)
  # g$layout
  # gtable::gtable_show_layout(g) # Might also be useful
  
  lyt <- g$layout
  lyt <- lyt %>% 
    tidyr::separate(name, sep = "-", into = c("panel", "row", "col"), remove = FALSE)
  
  panels <- grepl("^panel", lyt$name)
  
  # remove lower matrix triangle
  not_keep <- lyt %>% 
    filter(row == col)
  pos <- g$layout$name %in% with(not_keep, paste0("panel-", row, "-", col))
  g$grobs <- g$grobs[!pos]
  g$layout <- g$layout[!pos, ]
  
  # # Draw
  # grid::grid.newpage()
  #grid::grid.draw(g)
  ggplotify::as.ggplot(g)
}

remove_upper_panels <- function(x) {
  # Remove empty panels
  # Adapted from: https://en.it1352.com/article/d0a0d77cc54e4a2ea01ec2085831c390.html
  g <- ggplotGrob(x)
  # g$layout
  # gtable::gtable_show_layout(g) # Might also be useful
  
  lyt <- g$layout
  lyt <- lyt %>% 
    separate(name, sep = "-", into = c("panel", "row", "col"), remove = FALSE)
  
  panels <- grepl("^panel", lyt$name)
  
  dimension <- seq_along(unique(lyt$row[panels]))
  rw <-  matrix(dimension, nrow = max(dimension), ncol = max(dimension))
  cl <-  matrix(dimension, nrow = max(dimension), ncol = max(dimension), byrow = TRUE)
  # remove lower matrix triangle
  not_keep <- tibble(row = rw[upper.tri(rw)], col = cl[upper.tri(cl)])
  pos <- g$layout$name %in% with(not_keep, paste0("panel-", row, "-", col))
  g$grobs <- g$grobs[!pos]
  g$layout <- g$layout[!pos, ]
  
  # # Draw
  # grid::grid.newpage()
  #grid::grid.draw(g)
  ggplotify::as.ggplot(g)
}

my_plot_contrasts_single <- function(x, scale_r, remove_diag = TRUE) {
  pd <- my_plot_data_combined_global_envelope(x, scale_r = scale_r, split = TRUE)
  
  p <-  pd$ribbon %>% 
    filter(
      type == "Data function"
    ) %>% 
    ggplot(aes(x = r, y = curves, group = interaction(type, a, b))) + 
    geom_ribbon(aes(x = r, ymin = lower, ymax = upper), fill = envelope_grey, alpha = 0.8) +
    #geom_line(size = 0.3, alpha = 0.3) +
    #geom_point(size = 0.5) +
    geom_segment(aes(xend = x_end, yend = y_end, color = ifelse(!inside & !inside_lead, "outside", NA)), show.legend = FALSE) +
    xlab("Scaled Stand Height") +
    ylab("Scaled density difference") +
    coord_flip()
}


add_diag_panels <- function(x, y) {
  # Remove empty panels
  # Adapted from: https://en.it1352.com/article/d0a0d77cc54e4a2ea01ec2085831c390.html
  gx <- ggplotGrob(x)
  # gx$layout
  # gtable::gtable_show_layout(gx) # Might also be useful
  
  
  gy <- ggplotGrob(y)
  y_lyt <- gy$layout
  
  lyt <- gx$layout
  lyt <- lyt %>% 
    tidyr::separate(name, sep = "-", into = c("panel", "row", "col"), remove = FALSE)
  
  panels <- grepl("^panel", lyt$name)
  diago <- grepl("^panel", y_lyt$name)
  
  # replace lower matrix triangle
  gx$grobs[!is.na(lyt$row) & !is.na(lyt$col) & lyt$row == lyt$col] <- gy$grobs[diago]
  
  # # Draw
  # grid::grid.newpage()
  #grid::grid.draw(g)
  ggplotify::as.ggplot(gx)
}
