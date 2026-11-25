# Draws the fraction hex sticker. Run from the package root.

out_file <- "hex/fraction.svg"

# ---- Canvas and palette ------------------------------------------------------

R <- 300          # centre to outer corner
R_inner <- 275.5  # the edge is 8.2% of the half-width
gap <- 8          # pith around each segment
corner <- 12      # segment corner radius

pith <- "#fff6e0"
ink <- "#7a3606"
orange <- c(rind = "#e07b1f", pulp = "#f7a440", vesicle = "#ffc877")
lemon <- c(rind = "#e0ae1e", pulp = "#f9d754", vesicle = "#fdeb9c")
lime <- c(rind = "#5d8f31", pulp = "#a9cc57", vesicle = "#cfe592")

# ---- The fractions -----------------------------------------------------------

# fraction(1, 2) + fraction(1, 3) + fraction(1, 6) is 1.
# Corners are numbered clockwise from 0 at the top, so `from` to `to` spans
# (to - from) sixths of the hexagon.
slices <- list(
  list(from = 1, to = 4, fruit = orange, vesicles = FALSE), # 1/2, has the name
  list(from = 4, to = 6, fruit = lemon, vesicles = TRUE),   # 1/3
  list(from = 6, to = 7, fruit = lime, vesicles = TRUE)     # 1/6
)
stopifnot(sum(sapply(slices, function(s) s$to - s$from)) == 6)

# ---- Geometry ----------------------------------------------------------------

corners <- function(k, r) r * cbind(sin(k * pi / 3), -cos(k * pi / 3))

# A convex polygon's edges as lines (unit normal, offset), moved inwards by d
edges <- function(m, d = 0) {
  b <- m[c(2:nrow(m), 1), ]
  u <- (b - m) / sqrt(rowSums((b - m)^2))
  normal <- cbind(-u[, 2], u[, 1])
  cbind(normal, rowSums(normal * m) + d)
}
inset <- function(m, d) {
  e <- edges(m, d)
  prev <- c(nrow(e), seq_len(nrow(e) - 1))
  t(sapply(seq_len(nrow(e)), function(i) {
    l <- e[c(prev[i], i), ]
    solve(l[, 1:2], l[, 3])
  }))
}
inside <- function(p, m, d) {
  e <- edges(m, d)
  all(e[, 1:2] %*% p > e[, 3])
}

# ---- SVG helpers -------------------------------------------------------------

fmt <- function(v) sub("\\.?0+$", "", sprintf("%.2f", v))
polygon <- function(m, fill, stroke = 1) {
  sprintf('<polygon points="%s" fill="%s" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
          paste0(fmt(m[, 1]), ",", fmt(m[, 2]), collapse = " "), fill, fill, stroke)
}
dash <- function(a, b, col) {
  sprintf('<path d="M%s,%s L%s,%s" stroke="%s" stroke-width="7" stroke-linecap="round"/>',
          fmt(a[1]), fmt(a[2]), fmt(b[1]), fmt(b[2]), col)
}

# ---- Fruit -------------------------------------------------------------------

slice <- function(s) {
  k <- s$from:s$to
  m <- corners(k, R_inner)
  if (length(k) < 4) m <- rbind(c(0, 0), m) # a half has no corner at the centre
  c(
    polygon(rbind(corners(k, R), corners(rev(k), R_inner)), s$fruit[["rind"]], 0),
    polygon(inset(m, gap + corner), s$fruit[["pulp"]], 2 * corner),
    if (s$vesicles) vesicles(m, s$fruit[["vesicle"]])
  )
}

# Dashes pointing at the centre, on alternating rings
vesicles <- function(m, col, half = 8) {
  unlist(lapply(0:5, function(ring) {
    r <- 66 + 36 * ring
    n <- 6 * max(1, round(2 * pi * r / 46 / 6))
    shift <- if ((r / 36) %% 2 < 1) 0 else -.5
    unlist(lapply((seq_len(n) + shift) * 2 * pi / n, function(a) {
      u <- c(sin(a), -cos(a))
      from <- (r - half) * u
      to <- (r + half) * u
      if (inside(from, m, gap + 9) && inside(to, m, gap + 9)) dash(from, to, col)
    }))
  }))
}

# ---- Wordmark ----------------------------------------------------------------

# "fraction" in Lexend SemiBold at 100px, as outlines so no font is needed
word <- paste0(
  "M 11.2,100 V 43.4 q 0,-4.9 2.2,-8.9 2.2,-4 6,-6.3 3.9,-2.3 9.1,-2.3 3.5,0 6.6,1.2 ",
  "3.1,1.1 5.3,3.1 l -4.2,10.4 q -1.3,-0.8 -2.7,-1.3 -1.3,-0.6 -2.4,-0.6 -1.9,0 -3.2,0.7 ",
  "-1.2,0.6 -1.8,1.9 -0.6,1.2 -0.6,3.1 V 100 h -7.1 q -2.3,0 -4.1,0 -1.8,0 -3.1,0 z M ",
  "3.3,60.4 v -12 h 33.6 v 12 z M 46.4,100 V 47.1 h 13.7 l 0.5,17 -2.4,-3.5 q 1.2,-4.1 ",
  "3.9,-7.4 2.7,-3.4 6.3,-5.3 3.7,-1.9 7.7,-1.9 1.7,0 3.3,0.3 1.6,0.3 2.7,0.7 l -3.8,15.6 ",
  "q -1.2,-0.6 -2.9,-1 -1.7,-0.5 -3.5,-0.5 -2.4,0 -4.5,0.9 -2,0.8 -3.5,2.4 -1.5,1.5 ",
  "-2.4,3.6 -0.8,2.1 -0.8,4.6 V 100 Z M 107.8,101 q -6.6,0 -11.9,-3.6 -5.3,-3.6 -8.4,-9.8 ",
  "-3.1,-6.2 -3.1,-14.2 0,-8 3.1,-14.1 3.2,-6.2 8.6,-9.7 Q 101.5,46 108.4,46 q 3.9,0 ",
  "7.1,1.2 3.3,1.1 5.7,3.1 2.5,2 4.2,4.6 1.7,2.6 2.4,5.6 l -3,-0.5 V 47.1 h 14.1 V 100 H ",
  "124.6 V 87.3 l 3.2,-0.3 q -0.8,2.8 -2.6,5.3 -1.8,2.5 -4.5,4.5 -2.6,1.9 -5.9,3.1 ",
  "-3.3,1.1 -7,1.1 z m 3.9,-12.3 q 4,0 7,-1.9 3,-1.9 4.6,-5.3 1.7,-3.5 1.7,-8.1 0,-4.5 ",
  "-1.7,-7.9 -1.6,-3.4 -4.6,-5.3 -3,-2 -7,-2 -3.9,0 -6.9,2 -2.9,1.9 -4.6,5.3 -1.7,3.4 ",
  "-1.7,7.9 0,4.6 1.7,8.1 1.7,3.4 4.6,5.3 3,1.9 6.9,1.9 z m 64.4,12.3 q -7.6,0 -13.6,-3.6 ",
  "-6,-3.6 -9.5,-9.8 -3.4,-6.2 -3.4,-14.1 0,-7.8 3.4,-14 3.5,-6.3 9.5,-9.9 6,-3.6 ",
  "13.6,-3.6 7.1,0 13,2.6 5.9,2.6 9.2,7.2 l -7.8,9.4 q -1.4,-1.9 -3.5,-3.4 -2.1,-1.6 ",
  "-4.6,-2.5 -2.4,-0.9 -5,-0.9 -4.1,0 -7.3,2 -3.1,1.9 -4.9,5.3 -1.8,3.4 -1.8,7.8 0,4.3 ",
  "1.8,7.7 1.9,3.3 5.1,5.4 3.2,2 7.2,2 2.6,0 4.9,-0.7 2.3,-0.8 4.3,-2.3 2.1,-1.5 3.8,-3.6 ",
  "l 7.7,9.4 q -3.4,4.3 -9.4,7 -6,2.6 -12.7,2.6 z m 36.3,-1 V 33.7 h 14.2 V 100 Z m ",
  "-9.9,-40.2 V 47.1 h 34.9 V 59.8 Z M 247.9,100 V 47.1 h 14.2 V 100 Z m 7,-63.8 q -4.1,0 ",
  "-6.4,-2 -2.3,-2 -2.3,-5.7 0,-3.4 2.3,-5.5 2.4,-2.2 6.4,-2.2 4.1,0 6.4,2.1 2.3,2 ",
  "2.3,5.6 0,3.5 -2.4,5.6 -2.3,2.1 -6.3,2.1 z m 47.0,64.8 q -8.1,0 -14.5,-3.5 -6.3,-3.6 ",
  "-10,-9.8 -3.6,-6.2 -3.6,-14.2 0,-8 3.6,-14.1 3.7,-6.2 10,-9.8 6.4,-3.6 14.5,-3.6 8,0 ",
  "14.3,3.6 6.4,3.6 10,9.8 3.6,6.1 3.6,14.1 0,8 -3.6,14.2 -3.6,6.2 -10,9.8 -6.3,3.5 ",
  "-14.3,3.5 z m 0,-12.4 q 3.9,0 7,-1.9 3.1,-2 4.8,-5.4 1.8,-3.5 1.8,-7.8 0,-4.4 ",
  "-1.8,-7.8 -1.7,-3.5 -4.8,-5.4 -3.1,-2 -7,-2 -4,0 -7.1,2 -3.1,2 -4.9,5.4 -1.8,3.4 ",
  "-1.7,7.8 -0.1,4.3 1.7,7.8 1.8,3.4 4.9,5.4 3.1,1.9 7.1,1.9 z M 340.6,100 V 47.1 h 13.6 ",
  "l 0.4,10.8 -2.8,1.2 q 1.1,-3.6 3.9,-6.5 2.9,-3 6.9,-4.8 4,-1.8 8.4,-1.8 6,0 10,2.4 ",
  "4.1,2.4 6.1,7.3 2.1,4.8 2.1,11.9 V 100 h -14.2 V 68.7 q 0,-3.6 -1,-6 -1,-2.4 -3.1,-3.5 ",
  "-2,-1.2 -5,-1 -2.4,0 -4.5,0.8 -2,0.7 -3.5,2.1 -1.4,1.4 -2.3,3.2 -0.8,1.8 -0.8,3.9 V ",
  "100 h -7 q -2.3,0 -4.1,0 -1.8,0 -3.1,0 z "
)
wordmark <- function(size = 78, depth = 127) {
  sprintf('<path transform="translate(%s %s) rotate(-30) scale(%s) translate(-197.2 -74)" fill="%s" d="%s"/>',
          fmt(depth / 2), fmt(depth * sqrt(3) / 2), size / 100, ink, trimws(word))
}

# ---- Sticker -----------------------------------------------------------------

svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-260 -300 520 600" role="img" aria-label="fraction hex sticker">',
  polygon(corners(0:5, R_inner), pith),
  paste(unlist(lapply(slices, slice)), collapse = ""),
  wordmark(),
  "</svg>"
)
writeLines(svg, out_file)
