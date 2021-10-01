library(magrittr);

render <- function(A, B, screen_height, screen_width, theta_spacing,
  phi_spacing, R1, R2, K1, K2) {

  cosA <- cos(A);
  sinA <- sin(A);
  cosB <- cos(B);
  sinB <- sin(B);

  ascii_out <- c();
  zbuffer <- c();

  lapply(
    seq_len(screen_height + 1),
    function(i) {
      ascii_out <<- ascii_out %>%
        rbind(rep(" ", screen_width));
      zbuffer <<- zbuffer %>%
        rbind(rep(0, screen_width))
    }
  )

  theta <- 0;

  while (theta <= 2 * pi) {
    theta <- theta + theta_spacing;
    costheta <- cos(theta);
    sintheta <- sin(theta);

    phi <- 0;
    while (phi <= 2 * pi) {
      phi <- phi + phi_spacing;
      cosphi <- cos(phi);
      sinphi <- sin(phi);

      circlex <- R2 + R1 * costheta;
      circley <- R1 * sintheta;

      x <- circlex * (cosB * cosphi + sinA * sinB * sinphi) -
        circley * cosA * sinB;
      y <- circlex * (sinB * cosphi - sinA * cosB * sinphi) +
        circley * cosA * cosB;
      z <- K2 + cosA * circlex * sinphi + circley * sinA;
      ooz <- 1 / z;

      xp <- as.integer(screen_width / 2 + K1 * ooz * x);
      yp <- as.integer(screen_height / 2 - K1 * ooz * y);

      L <- cosphi * costheta * sinB - cosA * costheta * sinphi -
        sinA * sintheta + cosB * (cosA * sintheta - costheta * sinA * sinphi);
      if (L > -1 && L < 0) L <- 0L;

      if (L >= 0) {
        zbuffer[xp, yp] <- ooz;
        luminance_index <- as.integer(L * 8) + 1L;
        char_index <- c(
          ".", ",", "-", "~", ":", ";", "=", "!", "*", "#", "$", "@"
        );
        ascii_out[yp, xp] <- char_index[luminance_index];
      }
    }
  }
  message("\x1b[H");
  lapply(
    seq_len(nrow(ascii_out)),
    function(i) {
      paste(ascii_out[i, ], collapse = "")
    }
  ) %>%
    paste(collapse = "\n") %>%
    message();
}

R1 <- 1;
R2 <- 2;
K2 <- 5;

screen_width <- 35;
screen_height <- 35;

K1 <- screen_width * K2 * 3 / (8 * (R1 + R2));

message("\x1b[2J");
A <- 1;
B <- 1;

while (TRUE) {
  render(
    A = A,
    B = B,
    screen_height = screen_height,
    screen_width = screen_width,
    theta_spacing = 0.07,
    phi_spacing = 0.02,
    R1 = R1,
    R2 = R2,
    K1 = K1,
    K2 = K2
  );
  A <- A + 0.08;
  B <- B + 0.03;
}

