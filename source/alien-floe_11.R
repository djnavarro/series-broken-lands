
floe <- function(seed) {

  library(rayshader)
  library(tibble)
  library(ambient)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(dplyr)
  library(tictoc)
  library(here)
  library(ragg)

  sample_canva2 <- function(seed = NULL, n_steps = 4, n_base = 4) {

    if(!is.null(seed)) set.seed(seed)
    sample(ggthemes::canva_palettes, 1)[[1]][1:n_base] |>
      (\(x) colorRampPalette(x)(n_steps))()
  }

  transform_to_curl_space <- function(x, y) {
    curl_noise(
      generator = fracture,
      noise = gen_simplex,
      fractal = fbm,
      octaves = 3,
      frequency = ~ . * 2,
      freq_init = .3,
      gain_init = 1,
      gain = ~ . * .5,
      x = x,
      y = y
    )
  }

  discretise <- function(x, n) {
    round(x * n) / n
  }

  define_worley_cells <- function(x, y) {
    fracture(
      noise = gen_worley,
      fractal = billow,
      octaves = 8,
      freq_init = .1,
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


  simplex_noise <- function(x, y) {
    fracture(
      noise = gen_simplex,
      fractal = billow,
      octaves = 10,
      freq_init = .02,
      frequency = ~ . * 2,
      gain_init = 1,
      gain = ~ . * .8,
      x = x,
      y = y
    ) |>
      normalise()
  }


  ice_floe <- function(seed) {

    set.seed(seed)

    s <- 1
    grid <- long_grid(
      x = seq(0, s, length.out = 2000),
      y = seq(0, s, length.out = 2000)
    )

    coords <- transform_to_curl_space(grid$x, grid$y)

    grid |>
      mutate(
        cells = define_worley_cells(coords$x, coords$y),
        paint = simplex_noise(x + cells, y + cells),
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

    shades <- sample_canva2(seed, n_steps = 4, n_base = 4)
    bg <- shades[4]
    fg <- colorRampPalette(shades[1:3])(40)
    shades <- c(rep(bg, 2), sample(fg))

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

  sys_id <- "11"
  fname <- paste0("alien-floe_", sys_id, "_", seed, ".png")
  cat("generating", fname, "\n")
  if(!dir.exists(here("output", sys_id))) {
    dir.create(here("output"), sys_id)
  }
  agg_png(
    filename = here("output", sys_id, fname),
    width = 2000,
    height = 2000
  )
  alien_floe(seed)
  dev.off()

}

for(seed in 2001:2050) {
  floe(seed)
}
