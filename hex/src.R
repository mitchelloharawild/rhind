# Draws the rhind hex sticker. Run from the package root.

out_file <- "hex/rhind.svg"

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

# "rhind" in Lexend SemiBold at 100px, as outlines so no font is needed
word <- paste0(
  "M 7.1,100 V 47.1 h 13.7 l 0.5,17 -2.4,-3.5 q 1.2,-4.1 3.9,-7.4 2.7,-3.4 6.3,-5.3 ",
  "3.7,-1.9 7.7,-1.9 1.7,0 3.3,0.3 1.6,0.3 2.7,0.7 L 39,62.6 q -1.2,-0.6 -2.9,-1 -1.7,-0.5 ",
  "-3.5,-0.5 -2.4,0 -4.5,0.9 -2,0.8 -3.5,2.4 -1.5,1.5 -2.4,3.6 -0.8,2.1 -0.8,4.6 V 100 Z m ",
  "43.8,0 V 26 h 14 v 31.9 l -2.8,1.2 q 1.1,-3.6 3.9,-6.5 2.9,-3 6.9,-4.8 4,-1.8 8.4,-1.8 ",
  "6,0 10,2.4 4.1,2.4 6.1,7.3 2.1,4.8 2.1,11.9 V 100 h -14.2 V 68.7 q 0,-3.6 -1,-6 -1,-2.4 ",
  "-3.1,-3.5 -2,-1.2 -5,-1 -2.4,0 -4.5,0.8 -2,0.7 -3.5,2.1 -1.4,1.4 -2.3,3.2 -0.8,1.8 ",
  "-0.8,3.9 V 100 h -7 q -2.3,0 -4.1,0 -1.8,0 -3.1,0 z m 63.4,0 V 47.1 h 14.2 V 100 Z m ",
  "7,-63.8 q -4.1,0 -6.4,-2 -2.3,-2 -2.3,-5.7 0,-3.4 2.3,-5.5 2.4,-2.2 6.4,-2.2 4.1,0 ",
  "6.4,2.1 2.3,2 2.3,5.6 0,3.5 -2.4,5.6 -2.3,2.1 -6.3,2.1 z M 143.7,100 V 47.1 h 13.6 l ",
  "0.4,10.8 -2.8,1.2 q 1.1,-3.6 3.9,-6.5 2.9,-3 6.9,-4.8 4,-1.8 8.4,-1.8 6,0 10,2.4 ",
  "4.1,2.4 6.1,7.3 2.1,4.8 2.1,11.9 V 100 h -14.2 V 68.7 q 0,-3.6 -1,-6 -1,-2.4 -3.1,-3.5 ",
  "-2,-1.2 -5,-1 -2.4,0 -4.5,0.8 -2,0.7 -3.5,2.1 -1.4,1.4 -2.3,3.2 -0.8,1.8 -0.8,3.9 V 100 ",
  "h -7 q -2.3,0 -4.1,0 -1.8,0 -3.1,0 z m 82.9,1 q -6.9,0 -12.4,-3.5 -5.5,-3.6 -8.7,-9.7 ",
  "-3.1,-6.2 -3.1,-14.3 0,-7.9 3.1,-14.1 3.2,-6.2 8.6,-9.8 5.5,-3.6 12.5,-3.6 3.7,0 ",
  "7.1,1.2 3.5,1.1 6.2,3.1 2.8,2 4.5,4.5 1.7,2.4 1.9,5.1 l -3.6,0.6 V 26 h 14.2 v 74 h ",
  "-13.4 l -0.6,-12.2 2.8,0.3 q -0.2,2.5 -1.8,4.8 -1.6,2.3 -4.3,4.2 -2.6,1.8 -6,2.9 -3.3,1 ",
  "-7,1 z m 3.2,-11.8 q 4,0 7,-2 3,-2 4.7,-5.5 1.7,-3.5 1.7,-8.2 0,-4.6 -1.7,-8.1 ",
  "-1.7,-3.6 -4.7,-5.5 -3,-2 -7,-2 -4,0 -7,2 -2.9,1.9 -4.6,5.5 -1.6,3.5 -1.6,8.1 0,4.7 ",
  "1.6,8.2 1.7,3.5 4.6,5.5 3,2 7,2 z"
)
wordmark <- function(size = 110, depth = 124) {
  sprintf('<path transform="translate(%s %s) rotate(-30) scale(%s) translate(-132 -64)" fill="%s" d="%s"/>',
          fmt(depth / 2), fmt(depth * sqrt(3) / 2), size / 100, ink, trimws(word))
}

# ---- Sticker -----------------------------------------------------------------

svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-260 -300 520 600" role="img" aria-label="rhind hex sticker">',
  polygon(corners(0:5, R_inner), pith),
  paste(unlist(lapply(slices, slice)), collapse = ""),
  wordmark(),
  "</svg>"
)
writeLines(svg, out_file)
