SPACE <- " "
CHAR_INDEX <- c(".", ",", "-", "~", ":", ";", "=", "!", "*", "#", "$", "@")

render <- compiler::cmpfun(function(
  A, B, screen_height, screen_width,
  theta_seq,
  theta_cos_seq,
  theta_sin_seq,
  theta_seq_len,
  phi_seq,
  phi_cos_seq,
  phi_sin_seq,
  phi_seq_len,
  R1, R2, K1, K2,
  ascii_out,
  zbuffer
  ) {

  cos_a <- cos(A)
  sin_a <- sin(A)
  cos_b <- cos(B)
  sin_b <- sin(B)

  for (theta_i in theta_seq_len) {
    costheta <- theta_cos_seq[theta_i]
    sintheta <- theta_sin_seq[theta_i]
    for (phi_i in phi_seq_len) {
      cosphi <- phi_cos_seq[phi_i]
      sinphi <- phi_sin_seq[phi_i]

      circlex <- R2 + R1 * costheta
      circley <- R1 * sintheta

      x <- circlex * (cos_b * cosphi + sin_a * sin_b * sinphi) -
        circley * cos_a * sin_b
      y <- circlex * (sin_b * cosphi - sin_a * cos_b * sinphi) +
        circley * cos_a * cos_b
      z <- K2 + cos_a * circlex * sinphi + circley * sin_a
      ooz <- 1 / z

      xp <- as.integer(screen_width / 2 + K1 * ooz * x)
      yp <- as.integer(screen_height / 2 - K1 * ooz * y)

      l <- cosphi * costheta * sin_b - cos_a * costheta * sinphi -
        sin_a * sintheta + cos_b * (cos_a * sintheta - costheta * sin_a * sinphi)

      l <- l * !(l > -1 && l < 0)

      if (l >= 0) {
        zbuffer[xp, yp] <- ooz
        luminance_index <- as.integer(l * 8) + 1L

        ascii_out[xp, yp] <- CHAR_INDEX[luminance_index]
      }
    }
  }
  cat("\x1b[H", ascii_out)
  return()
})

R1 <- 1
R2 <- 2
K2 <- 5

screen_width <- 35
screen_height <- 35

K1 <- screen_width * K2 * 3 / (8 * (R1 + R2))

A <- 1
B <- 1

theta_spacing <- 0.07
phi_spacing <- 0.02

# Precompute inner and out loop seq
theta_seq <- seq(0, 2 * pi, by = theta_spacing)
theta_cos_seq <- cos(theta_seq)
theta_sin_seq <- sin(theta_seq)
theta_seq_len <- seq_along(theta_seq)

phi_seq <- seq(0, 2 * pi, by = phi_spacing)
phi_cos_seq <- cos(phi_seq)
phi_sin_seq <- sin(phi_seq)
phi_seq_len <- seq_along(phi_seq)

ascii_out <- matrix(SPACE, ncol = screen_height, nrow = screen_width + 1)
ascii_out[screen_width + 1, ] <- "\n"
zbuffer <- matrix(0.0, nrow = screen_height, ncol = screen_width)

run <- compiler::cmpfun(function() {
  cat("\x1b[2J")
  repeat {
    render(
      A = A,
      B = B,
      screen_height = screen_height,
      screen_width = screen_width,
      theta_seq = theta_seq,
      theta_cos_seq = theta_cos_seq,
      theta_sin_seq = theta_sin_seq,
      theta_seq_len = theta_seq_len,
      phi_seq = phi_seq,
      phi_cos_seq = phi_cos_seq,
      phi_sin_seq = phi_sin_seq,
      phi_seq_len = phi_seq_len,
      R1 = R1,
      R2 = R2,
      K1 = K1,
      K2 = K2,
      ascii_out = ascii_out,
      zbuffer = zbuffer
    )
    A <- A + 0.08
    B <- B + 0.03
  }
})

run()
