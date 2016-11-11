## Helper functions.

# ggplot2 theme.
theme_simple <- function(base_size = 18, text_size = 20) {
  bg_color <- "#ffffff"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size = base_size) +
    theme(axis.title = element_text(size = text_size),
          axis.text = element_text(color = "grey40"),
          axis.ticks = element_line(color = "grey40"),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_rect(fill = NA, color = "grey80"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey80", size = 0.25),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0, size = 24, color = "grey40"))
}

# ggplot2 map theme.
theme_map <- function(base_size = 18, text_size = 20) {
  bg_color <- "#002266"  # Dark blue for the oceans.
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size = base_size) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0, size = 24, color = "grey40"))
}

# A function to add a subtitle to a ggplot object.
# Thanks to Bob Rudis for the function!
ggplot_with_subtitle <- function(gg, label = "", fontfamily = NULL,
                                 fontsize = 10, hjust = 0, vjust = 0, 
                                 bottom_margin = 5.5, newpage = is.null(vp),
                                 vp = NULL, ...) {
  
  if (is.null(fontfamily)) {
    gpr <- grid::gpar(fontsize = fontsize, ...)
  } else {
    gpr <- grid::gpar(fontfamily = fontfamily, fontsize = fontsize, ...)
  }
  
  subtitle <- grid::textGrob(label, x = unit(hjust, "npc"),
                             y = unit(hjust, "npc"),
                             hjust = hjust, vjust = vjust, gp = gpr)
  
  data <- ggplot_build(gg)
  
  gt <- ggplot_gtable(data)
  gt <- gtable::gtable_add_rows(gt, grid::grobHeight(subtitle), 2)
  gt <- gtable::gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
  gt <- gtable::gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)
  
  if (newpage) grid::grid.newpage()
  
  if (is.null(vp)) {
    grid::grid.draw(gt)
  } else {
    if (is.character(vp)) grid::seekViewport(vp) else grid::pushViewport(vp)
    grid::grid.draw(gt)
    grid::upViewport()
  }
  
  invisible(data)
}