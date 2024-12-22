library(ggplot2)
library(gganimate)
library(dplyr)

# Function to generate the animated Hanukkah card plot
generate_hanukkah_plot <- function(greeting_text, output_file = "www/hanukkah_card.gif") {
  # Generate Data for Menorah Base
  menorah_base <- data.frame(
    x_start = -3,
    x_end = 3,
    y_base = -1.2  # Adjusted horizontal base line to match candle bottom
  )

  # Generate Data for Menorah Arms
  menorah_arms <- data.frame(
    x_start = c(-3, -2, -1, 0, 1, 2, 3),
    y_start = rep(-1.2, 7),
    x_end = c(-3, -2, -1, 0, 1, 2, 3),
    y_end = rep(0.0, 7)  # Candles above the base
  )

  # Generate Data for Candles
  candles <- data.frame(
    x = rep(c(-3, -2, -1, 0, 1, 2, 3), each = 2),
    y = rep(c(-1.2, 0.0), 7)  # Adjusted to align with the new base position
  )

  # Generate Data for Flames (Position Above Candle Tops)
  flames <- data.frame(
    x = c(-3, -2, -1, 0, 1, 2, 3),
    y = rep(1.1, 7),  # Flames just above the top of the candles
    frame = rep(1:10, each = 7),
    size = rep(c(5, 6), times = 35)  # Alternating sizes for twinkle effect
  )

  # Generate Data for Menorah Center Base
  center_base <- data.frame(
    x_start = 0,
    x_end = 0,
    y_start = -1.2,
    y_end = -2.0  # Vertical line for the base
  )

  # Generate Data for Small Bottom Base
  bottom_base <- data.frame(
    x_start = -0.2,
    x_end = 0.2,
    y_base = -2.0  # Horizontal small base
  )

  # Create the Animated Plot
  p <- ggplot() +
    # Menorah Base
    geom_segment(data = menorah_base, aes(x = x_start, y = y_base, xend = x_end, yend = y_base), size = 2, color = "blue") +
    # Menorah Arms
    geom_segment(data = menorah_arms, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), size = 1, color = "blue") +
    # Candles
    geom_segment(data = candles, aes(x = x, y = y, xend = x, yend = y + 1.2), size = 3, color = "blue") +
    # Flames
    geom_point(data = flames, aes(x = x, y = y + 0.2, group = frame, size = size), color = "orange", shape = 8) +
    # Menorah Center Base
    geom_segment(data = center_base, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), size = 3, color = "blue") +
    # Small Bottom Base
    geom_segment(data = bottom_base, aes(x = x_start, y = y_base, xend = x_end, yend = y_base), size = 2, color = "blue") +
    # Greeting Text
    annotate("text", x = 0, y = -2.5, label = greeting_text, size = 6, color = "darkblue", fontface = "bold") +  # Moved text
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
