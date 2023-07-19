
floe <- function(seed) {

  library(rayshader)
  library(tibble)
  library(ambient)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(ragg)

  # choose a palette either using ggthemes::canva_palettes
  # or colorir::colores
  sample_palette <- function(seed = NULL) {
    if(!is.null(seed)) set.seed(seed)
    if(runif(1) < .5) {
      return(sample(ggthemes::canva_palettes, 1)[[1]])
    }
    p <- sample(unique(colorir::colores$palette_name), 1)
    colorir::colores$colour[colorir::colores$palette_name == p]
  }

  # construct the curled manifold using a curl field
  transform_to_curl_space <- function(x, y) {
    curl_noise(
      generator = fracture,
      noise = gen_simplex,
      fractal = fbm,
      octaves = 3,
      frequency = ~ . * 2,
      freq_init = .2,
      gain_init = 1,
      gain = ~ . * .5,
      x = x,
      y = y
    )
  }

  # helper function to construct discrete levels
  discretise <- function(x, n) {
    round(x * n) / n
  }

  # use squared distance from worley cell centroid
  # to define a spatial noise pattern
  define_worley_cells <- function(x, y) {
    fracture(
      noise = gen_worley,
      fractal = billow,
      octaves = 8,
      freq_init = .05,
      frequency = ~ . * 2,
      gain_init = 3,
      gain = ~ . * .5,
      value = "distance2",
      x = x,
      y = y
    ) |>
      normalise() |>
      discretise(30)

  }

  # simplex noise because why not
  simplex_noise <- function(x, y) {
    fracture(
      noise = gen_simplex,
      fractal = billow,
      octaves = 10,
      freq_init = .01,
      frequency = ~ . * 2,
      gain_init = 1,
      gain = ~ . * .8,
      x = x,
      y = y
    ) |>
      normalise()
  }

  # helper function to choose the zoom level of the image
  sample_zoom <- function() {
    rbeta(1, shape1 = 2, shape2 = 1)*2.5 + .5
  }

  ice_floe <- function(seed) {

    set.seed(seed)

    zoom <- sample_zoom()
    grid <- long_grid(
      x = seq(0, zoom, length.out = 3000),
      y = seq(0, zoom, length.out = 3000)
    )

    coords <- transform_to_curl_space(grid$x, grid$y)

    a <- runif(1, -1, 1)
    b <- runif(1, -1, 1)
    grid |>
      mutate(
        cells = define_worley_cells(coords$x, coords$y),
        paint = simplex_noise(a * x + cells, b * y + cells),
        paint = normalise(paint)
      ) |>
      as.array(value = paint)
  }

  alien_floe <- function(seed) {

    art <- ice_floe(seed)
    art <- oce::matrixSmooth(art, passes = 6)
    art <- discretise(art, 80)
    cutoff <- median(art)
    art[art < cutoff] <- cutoff

    ncol <- length(unique(art))
    palette <- sample_palette(seed)
    bg <- palette[4]
    fg <- colorRampPalette(palette[1:3])(ncol - 1)
    shades <- c(bg, sample(fg))

    angle <- 90 + rnorm(1, 0, 30)

    height_shade(
      heightmap = art,
      texture = shades
    ) |>
      add_shadow(
        shadowmap = ray_shade(
          heightmap = art,
          sunaltitude = 50,
          sunangle = angle,
          multicore = TRUE,
          zscale = .001
        ),
        max_darken = .1
      ) |>
      plot_map()
  }

  sys_id <- "15"
  fname <- paste0("alien-floe_", sys_id, "_", seed, ".png")
  cat("generating", fname, "\n")
  if(!dir.exists(here("output", sys_id))) {
    dir.create(here("output"), sys_id)
  }
  agg_png(
    filename = here("output", sys_id, fname),
    width = 3000,
    height = 3000
  )
  alien_floe(seed)
  dev.off()

}

for(seed in 2451:2499) {
  floe(seed)
}
