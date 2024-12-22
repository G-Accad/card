library(ggplot2)
library(gganimate)
library(dplyr)

# Function to generate the animated Hanukkah card plot
generate_hanukkah_plot <- function(greeting_text, output_file = "www/hanukkah_card.gif") {
  # Generate Data for Menorah Base
  menorah_base <- data.frame(
    x_start = -3,
    x_end = 3,
    y_base = -0.5  # Adjusted horizontal base line
  )

  # Generate Data for Menorah Arms
  menorah_arms <- data.frame(
    x_start = c(-3, -2, -1, 0, 1, 2, 3),
    y_start = rep(-0.5, 7),
    x_end = c(-3, -2, -1, 0, 1, 2, 3),
    y_end = rep(1.2, 7)  # Shorter candles
  )

  # Generate Data for Diagonal Lines
  diagonal_lines <- menorah_arms %>%
    mutate(
      x_diag = 0,
      y_diag = -0.5  # Converging point for all diagonals at the base center
    )

  # Generate Data for Candles
  candles <- data.frame(
    x = rep(c(-3, -2, -1, 0, 1, 2, 3), each = 2),
    y = rep(c(-0.5, 1.2), 7)  # Shorter candles
  )

  # Generate Data for Flames (Adjusted Position Closer to Candle Tops)
  flames <- data.frame(
    x = c(-3, -2, -1, 0, 1, 2, 3),
    y = rep(1.3, 7),  # Flames just above candle tops
    frame = rep(1:10, each = 7),
    size = rep(c(5, 6), times = 35)  # Alternating sizes for twinkle effect
  )

  # Create the Animated Plot
  p <- ggplot() +
    # Menorah Base
    geom_segment(data = menorah_base, aes(x = x_start, y = y_base, xend = x_end, yend = y_base), size = 2, color = "blue") +
    # Menorah Arms
    geom_segment(data = menorah_arms, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), size = 1, color = "blue") +
    # Diagonal Lines
    geom_segment(data = diagonal_lines, aes(x = x_end, y = y_start, xend = x_diag, yend = y_diag), size = 1, color = "blue") +
    # Candles
    geom_segment(data = candles, aes(x = x, y = y - 1.7, xend = x, yend = y), size = 3, color = "blue") +
    # Flames
    geom_point(data = flames, aes(x = x, y = y, group = frame, size = size), color = "orange", shape = 8) +
    # Greeting Text
    annotate("text", x = 0, y = -0.8, label = greeting_text, size = 6, color = "darkblue", fontface = "bold") +
    # Customization
    theme_void() +
    theme(legend.position = "none") +
    # Animation settings
    transition_states(frame, transition_length = 1, state_length = 0.5) +
    enter_fade() +
    exit_fade()

  # Render and save the animation
  anim <- animate(p, nframes = 100, fps = 20, width = 600, height = 500, renderer = gifski_renderer(output_file))
  return(anim)
}
