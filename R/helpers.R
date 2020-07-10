# colors =======================================================================
light_grey <- "grey85"
dark_grey <- "grey75"
pal_cc <- RColorBrewer::brewer.pal(9, "YlOrRd")[-1]
pal_age <- RColorBrewer::brewer.pal(9, "BuPu")[-(1:2)]

# plot =========================================================================
theme_pub <- function(base_size = 14, base_family = "", ...){
  axis_line_size = 1
  axis_line_colour = "#222222"
  modifyList(theme_bw(base_size = base_size, base_family = base_family), 
             list(
               # remove background grid
               panel.grid.major = element_line(colour = NA), 
               panel.grid.minor = element_line(colour = NA),
               
               strip.background = element_rect(colour = NA),
               
               # remove facet label background 
               legend.key = element_rect(colour = NA),
               
               #                   axis.text.x=theme_text(size = base_size),
               #                   axis.text.y=theme_text(size = base_size),
               #                   axis.title.x=theme_text(size = base_size),
               #                   axis.title.y=theme_text(size = base_size, angle = 90),
               #                   plot.margin = unit(0.1 * c(-1, 1, 1, -1), "lines")
               
               #axis.line = element_blank(),
               #                  panel.background = theme_rect(size = axis_line_size, colour = NA),
               # axis.ticks = element_line(colour = axis_line_colour) #,
               #                   plot.title  = theme_text(size = base_size + 4, hjust = 0.01, vjust = -2, face = 'bold')
               
               # avoid accidental coma
               NULL
             ))
}
# move title :
# + opts(title = 'Graph Title', plot.title  = theme_text(colour = 'red', angle = 45, size = 10,hjust = 0.5, vjust = 0.5, face = 'bold'))

# renamer ======================================================================
dm_apply <- function(x, dm, as.factor. = FALSE){
  if(! all(as.character(x) %in% names(unlist(dm)))) {
    missing_values <- unique(x[which(!as.character(x) %in% names(unlist(dm)))])
    stop(paste("Missing values in domain: ", paste(missing_values, collapse = ", ")))
  }
  y <- sapply(as.character(x), function(xx) dm[[xx]])
  if(as.factor.) y <- as.factor(y)
  return(y)
}

# species domain
dm_sp <- {
  # this list was adapted from MRN documentation (data/mrn/essence_naipf)
  l <- list()
  l["BJ"] = "yellow birch"  # betula aleghaniensis
  l["BP"] = "paper birch"   # betula papyrifera
  l["EB"] = "white spruce"  # picea glauca
  l["EN"] = "black spruce"  # picea mariana
  l["EP"] = "spruce"        # spruce spp.
  l["ER"] = "maple"         # acer spp.
  l["ES"] = "sugar maple"   # acer saccharum
  l["EO"] = "red maple"     # acer rubrum
  l["EV"] = "picea abies"   # picea abies
  l["FI"] = "shade intolerant deciduous"
  l["FN"] = "non commercial deciduous"
  l["FX"] = "unidentified deciduous"
  l["FZ"] = "unidentified planted deciduous"  
  l["ML"] = "tamarack larch"  # larix laricina
  l["MH"] = "hybrid larch"  # larix laricina
  l["PE"] = "aspen"         # Populus spp. 
  l["PG"] = "Jack pine"     # pinus Banksiana
  l["PR"] = "red pine"      # pinus resinosa
  l["RX"] = "unidentified conifer" 
  l["RZ"] = "planted unidentified conifer"
  l["SB"] = "balsam fir"    # abies balsamea
  l["SE"] = "white spruce and balsam fir group"
  l["TO"] = "thuya occidentalis"
  l["  "] = NA
  l
}

# fda generate
get_type <- function(data, type, id = c("pee_no_acq")){
  f <- grep(type, names(data), value = TRUE)
  x <- data[, c(id, f)]
}
