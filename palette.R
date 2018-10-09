#creating a PA colour palette
pacman::p_load(ggplot2)

# Vector of PA colours
pa_colours <- c(`dark plum` = "#3C233E", 
                `plum` = "#6F5671", 
                `light plum` = "#9A889B", 
                `dark teal` = "#267280", 
                `teal` = "#518E99", 
                `light teal` = "#85AFB7",
                `dark blue` = "#307298",
                `light blue` = "#02C0EF",
                `yellow` = "#F6E268",
                `red` = "#DE2629")

# Fn to call pa colours
pa_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (pa_colours)
  
  pa_colours[cols]
}


# Define some pallettes
pa_palettes <- list(
  `main` = pa_cols("plum", "dark blue","teal"),
  
  `teal`  = pa_cols("light teal", "teal", "dark teal"),
  
  `plum`  = pa_cols("light plum", "plum", "dark plum"),
  
  `blue`  = pa_cols("light blue", "dark blue"),
  
  `cool` = pa_cols("plum", "light blue", "dark teal" ),
  
  `hot` = pa_cols("teal", "yellow", "red"),
  
  `mixed` = pa_cols("plum", "light blue", "teal", "yellow"),
  
  `rainbow` = pa_cols("plum", "light blue", "teal", "yellow", "red")
)


# Fn to access palette colours
pa_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- pa_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#test
pa_pal("mixed")(10)


# Fn to create a colour scale
scale_color_pa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("pa_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


# Fn to create a fill scale
scale_fill_pa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("pa_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#add in examples with uniform height bargraphs showing colours
#example for continuous scales


# variable 

n = 6 
pal1 = pa_pal("mixed")(n)
pal2 = pa_pal("hot")(n)
pal3 = pa_pal("main")(n)
pal4 = pa_pal("cool")(n)
pal5 = pa_pal("rainbow")(n)

ds = data.frame(y = runif(n, 1, 10), x = as.factor(1:n))

g = ggplot(ds) + geom_col(aes(x = x, y = y, fill = x)) 
g + scale_fill_manual(values = pal1)
g + scale_fill_manual(values = pal2)
g + scale_fill_manual(values = pal3)
g + scale_fill_manual(values = pal4)
g + scale_fill_manual(values = pal5)


# fixed 
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_color_pa("hot")

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_color_pa(discrete = FALSE, palette = "blue")

ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_pa(palette = "hot", guide = "none")


ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_pa(palette = "main", guide = "none")


ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_pa(palette = "rainbow", guide = "none")



ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_pa(palette = "hot", guide = "none")
