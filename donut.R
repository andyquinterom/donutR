# Configurable parameters ----------------------------------------------------
frames <- 500
SPACE <- " "
CHAR_INDEX <- c(".", ",", "-", "~", ":", ";", "=", "!", "*", "#", "$", "@")
r1 <- 1
r2 <- 2
k2 <- 5
screen_width <- 35
screen_height <- 35
k1 <- screen_width * k2 * 3 / (8 * (r1 + r2))
theta_spacing <- 0.07
phi_spacing <- 0.02

# Precompute inner and out loop seq ------------------------------------------
theta_seq <- seq(0, 2 * pi, by = theta_spacing)
theta_cos_seq <- cos(theta_seq)
theta_sin_seq <- sin(theta_seq)
theta_seq_len <- seq_along(theta_seq)
circle_x_seq <- r2 + r1 * theta_cos_seq
circle_y_seq <- r1 * theta_sin_seq
phi_seq <- seq(0, 2 * pi, by = phi_spacing)
phi_cos_seq <- cos(phi_seq)
phi_sin_seq <- sin(phi_seq)
phi_seq_len <- seq_along(phi_seq)

ascii_out <- matrix(SPACE, ncol = screen_height, nrow = screen_width + 1)
ascii_out[screen_width + 1, ] <- "\n"
file <- stdout()

build_renderer <- function(
  screen_height, screen_width,
  r1, r2, k1, k2
) {
  # Here we are the compiler, we are inlining constants into the code.
  code <- sprintf(
    r"{
    function(a, b) {

      file <- file
      ascii_out <- ascii_out 
      theta_seq <- theta_seq
      theta_cos_seq <- theta_cos_seq
      theta_sin_seq <- theta_sin_seq
      theta_seq_len <- theta_seq_len
      phi_seq <- phi_seq
      phi_cos_seq <- phi_cos_seq
      phi_sin_seq <- phi_sin_seq
      phi_seq_len <- phi_seq_len
      CHAR_INDEX <- CHAR_INDEX

      cos_a <- cos(a)
      sin_a <- sin(a)
      cos_b <- cos(b)
      sin_b <- sin(b)

      for (theta_i in theta_seq_len) {
        costheta <- theta_cos_seq[theta_i]
        sintheta <- theta_sin_seq[theta_i]
        circlex <- circle_x_seq[theta_i]
        circley <- circle_y_seq[theta_i]
        for (phi_i in phi_seq_len) {
          cosphi <- phi_cos_seq[phi_i]
          sinphi <- phi_sin_seq[phi_i]


          x <- circlex * (cos_b * cosphi + sin_a * sin_b * sinphi) -
            circley * cos_a * sin_b
          y <- circlex * (sin_b * cosphi - sin_a * cos_b * sinphi) +
            circley * cos_a * cos_b
          z <- %f + cos_a * circlex * sinphi + circley * sin_a
          ooz <- 1 / z

          xp <- %f / 2 + %f * ooz * x
          yp <- %f / 2 - %f * ooz * y

          l <- cosphi * costheta * sin_b - cos_a * costheta * sinphi -
            sin_a * sintheta + cos_b * (cos_a * sintheta - costheta * sin_a * sinphi)

          # This saves us an if!
          l <- l * !(l > -1 && l < 0)

          if (l >= 0) {
            ascii_out[xp, yp] <- CHAR_INDEX[l * 8 + 1L]
          }
        }
      }
      # Calling internal directly to avoid extra checks
      .Internal(cat(list("\x1b[H", ascii_out), file, "", FALSE, NULL, FALSE))
    }
    }",
    k2,
    screen_width,
    k1,
    screen_height,
    k1
  )
  render <- eval(parse(text = code))
  render
  compiler::cmpfun(
    render,
    options = list(
      optimize = 3L
    )
  )
}


run_fps <- compiler::cmpfun(function() {
  render <- build_renderer(
    screen_height = screen_height,
    screen_width = screen_width,
    r1 = r1,
    r2 = r2,
    k1 = k1,
    k2 = k2
  )
  a <- 1
  b <- 1
  cat("\x1b[2J")
  start <- Sys.time()
  for (i in 0:frames) {
    render(a = a, b = b)
    a <- a + 0.08
    b <- b + 0.03
  }
  elapsed <- Sys.time() - start
  cat("FPS:", frames / as.double(elapsed), "\n")
})

run_fps()
