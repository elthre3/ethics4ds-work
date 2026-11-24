# Figures shared by slides.qmd and notes.qmd (week 9). Sourced after each file's setup
# chunk, which defines the colors (navy, slate, cgray, khaki, gold) and draw_dag().
# `ts` is the size of text drawn inside a plot: larger on slides than in the notes.

# Numbers used in the text, checked where they are computed.
cover <- function(n, ratio) { r <- 1 / sqrt(1 + n * ratio); 2 * pnorm(1.96 * r) - 1 }
ratio <- 0.462                     # tau^2 / sigma^2, chosen so that coverage at 25 items is 42%
m_K <- function(K) integrate(function(x) x * K * dnorm(x) * pnorm(x)^(K - 1), -Inf, Inf)$value
words <- 15e12 * 0.75              # Llama 3: about 15 trillion tokens; about 0.75 words a token
uk_year <- 1105.2e6 * 52.18        # ONS: total actual weekly hours worked, UK; weeks in a year
stopifnot(abs(cover(25, ratio) - 0.42) < 0.005, abs(cover(400, ratio) - 0.114) < 0.005,
          abs(2 * pnorm(1.96 / 2) - 1 - 0.673) < 0.001,
          abs(1 + 25 * ratio - 12.6) < 0.05, abs(1 + 400 * ratio - 185.8) < 0.05,
          all(abs(sapply(c(2, 10, 27, 50), m_K) - c(0.564, 1.539, 1.998, 2.249)) < 0.002),
          abs(m_K(2) - 1 / sqrt(pi)) < 1e-6,
          abs(words - 1.125e13) < 1e9, abs(words / 1000 - 1.1e10) < 0.05e10,
          abs(words / 100 - 1.1e11) < 0.05e11, abs(uk_year - 5.77e10) < 0.01e10,
          abs(words / 1000 / uk_year - 0.2) < 0.01, abs(words / 100 / uk_year - 1.95) < 0.01,
          abs(words / 1000 / 1e8 - 112) < 1, abs(words / 100 / 1e8 - 1125) < 1,
          abs(words / 1000 * 15 / 1e9 - 169) < 1, abs(words / 100 * 15 / 1e12 - 1.69) < 0.01)

fig_timeline <- function(ts = 5) {
  ev <- data.frame(year = c(1950, 1966, 2000, 2012, 2022),
                   label = c("Turing's\nimitation game", "ELIZA", "machine learning",
                             "deep learning", "large language\nmodels"),
                   y = c(1, -1, 1, -1, 1))
  bands <- data.frame(start = c(1974, 1987, 1994, 2017), end = c(1980, 1993, 2011, 2025),
                      lane = c("winter", "winter", "ml", "llm"))
  ggplot() +
    geom_rect(data = subset(bands, lane == "winter"), aes(xmin = start, xmax = end, ymin = -0.35, ymax = 0.35),
              fill = cgray, alpha = 0.45) +
    geom_rect(data = subset(bands, lane != "winter"), aes(xmin = start, xmax = end, ymin = -0.12, ymax = 0.12),
              fill = gold, alpha = 0.8) +
    annotate("text", x = 1983.5, y = -0.6, label = "AI winters", size = ts, color = slate) +
    geom_hline(yintercept = 0, color = navy, linewidth = 0.8) +
    geom_segment(data = ev, aes(x = year, xend = year, y = 0, yend = 0.55 * y), color = navy) +
    geom_point(data = subset(ev, year %in% c(1950, 1966, 2012)), aes(year, 0), color = navy, size = 3) +
    geom_text(data = ev, aes(year, 0.85 * y, label = label), size = ts, lineheight = 0.9) +
    scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(1942, 2030)) +
    scale_y_continuous(limits = c(-1.35, 1.35)) +
    theme_void() +
    theme(axis.text.x = element_text(size = 3 * ts, color = slate))
}

fig_targets <- function(ts = 5) {
  set.seed(9)
  cells <- expand.grid(rel = c("reliable", "not reliable"), val = c("valid", "not valid"),
                       stringsAsFactors = FALSE)
  pts <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    s <- if (cells$rel[i] == "reliable") 0.1 else 0.32
    c0 <- if (cells$val[i] == "valid") c(0, 0) else c(0.45, 0.4)
    data.frame(panel = paste0(cells$rel[i], ", ", cells$val[i]),
               x = c0[1] + rnorm(12, 0, s), y = c0[2] + rnorm(12, 0, s))
  }))
  ring <- do.call(rbind, lapply(c(0.33, 0.66, 1), function(r)
    data.frame(r = r, x = r * cos(seq(0, 2 * pi, length.out = 100)), y = r * sin(seq(0, 2 * pi, length.out = 100)))))
  lev <- c("reliable, valid", "reliable, not valid", "not reliable, valid", "not reliable, not valid")
  pts$panel <- factor(pts$panel, levels = lev)
  ring <- merge(ring, data.frame(panel = factor(lev, levels = lev)))
  ggplot() +
    geom_path(data = ring, aes(x, y, group = r), color = cgray) +
    geom_point(data = subset(pts, abs(x) < 1.15 & abs(y) < 1.15), aes(x, y), color = navy, size = 0.6 * ts) +
    facet_wrap(~ panel, nrow = 1) +
    coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    theme_void() + theme(strip.text = element_text(size = 2.6 * ts, margin = margin(b = 4)))
}

fig_clock <- function() {
  th <- seq(0, 2 * pi, length.out = 200)
  ticks <- data.frame(a = (0:11) * pi / 6)
  hand <- function(frac, len) c(len * sin(2 * pi * frac), len * cos(2 * pi * frac))
  h <- hand((10 + 10 / 60) / 12, 0.5); m <- hand(10 / 60, 0.8)
  ggplot() +
    geom_path(aes(cos(th), sin(th)), color = navy, linewidth = 1.5) +
    geom_segment(data = ticks, aes(x = 0.85 * sin(a), y = 0.85 * cos(a), xend = 0.97 * sin(a), yend = 0.97 * cos(a)),
                 color = navy, linewidth = 1) +
    annotate("segment", x = 0, y = 0, xend = h[1], yend = h[2], linewidth = 2.2, color = navy, lineend = "round") +
    annotate("segment", x = 0, y = 0, xend = m[1], yend = m[2], linewidth = 1.4, color = navy, lineend = "round") +
    annotate("point", x = 0, y = 0, size = 3, color = gold) +
    coord_fixed() + theme_void()
}

fig_clockbench <- function(ts = 5) {
  d <- data.frame(when = rep(c("September 2025", "November 2026"), each = 2),
                  who = rep(c("people (average)", "best model"), 2),
                  acc = c(89.1, 13.3, 90.7, 87.8))
  d$when <- factor(d$when, levels = c("September 2025", "November 2026"))
  d$who <- factor(d$who, levels = c("people (average)", "best model"))
  ggplot(d, aes(when, acc, fill = who)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_text(aes(label = paste0(acc, "%")), position = position_dodge(0.8), vjust = -0.4, size = ts) +
    scale_fill_manual(values = c(navy, gold), name = NULL) +
    scale_y_continuous(limits = c(0, 105), breaks = c(0, 50, 100), labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "top", panel.grid.major.x = element_blank())
}

fig_coverage <- function(ts = 5) {
  cv <- data.frame(n = 10^seq(log10(5), log10(2000), length.out = 200))
  cv$coverage <- cover(cv$n, ratio)
  pts <- data.frame(n = c(25, 400)); pts$coverage <- cover(pts$n, ratio)
  pts$label <- sprintf("%d items: %.0f%%", pts$n, 100 * pts$coverage)
  ggplot(cv, aes(n, coverage)) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = cgray) +
    geom_line(color = navy, linewidth = 1.1) +
    geom_point(data = pts, color = gold, size = 0.8 * ts) +
    geom_text(data = pts, aes(label = label), hjust = -0.12, vjust = -0.7, size = ts) +
    scale_x_log10(breaks = c(5, 25, 100, 400, 2000)) +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1)) +
    labs(x = "items in the benchmark", y = "coverage")
}

fig_verifiable <- function(ts = 5) {
  d <- data.frame(x = 1:7,
                  label = c("arithmetic with\nan answer key", "a sudoku", "code with\ntests",
                            "a summary", "the facts\nin an essay", "a new diet's\neffect", "what is fair"),
                  y = c(1, -1, 1, -1, 1, -1, 1))
  ggplot(d) +
    annotate("segment", x = 0.6, xend = 7.4, y = 0, yend = 0, color = navy, linewidth = 0.9,
             arrow = arrow(length = unit(0.15, "inches"), ends = "both")) +
    geom_point(aes(x, 0), color = gold, size = 0.8 * ts) +
    geom_text(aes(x, 0.55 * y, label = label), size = ts, lineheight = 0.9) +
    annotate("text", x = 0.6, y = -1.45, label = "checked by a program", hjust = 0, size = ts, color = slate) +
    annotate("text", x = 7.4, y = -1.45, label = "needs judgment, or years", hjust = 1, size = ts, color = slate) +
    coord_cartesian(ylim = c(-1.6, 1.1)) +
    theme_void()
}

benchmark_nodes <- data.frame(
  name = c("benchmark items", "prompt, judge, settings", "tuning to the benchmark",
           "score", "capability", "performance in use", "tasks in use"),
  x = c(0, 0, 0, 3.3, 4.7, 6.1, 8.4),
  y = c(0, 0.9, 1.8, 0.9, 2.0, 0.9, 1.8))
benchmark_edges <- data.frame(
  from = c("benchmark items", "prompt, judge, settings", "tuning to the benchmark",
           "capability", "capability", "tasks in use", "tuning to the benchmark"),
  to   = c("score", "score", "score", "score", "performance in use", "performance in use", "capability"))
fig_benchmark_graph <- function(highlight = FALSE, ts = 4.4) {
  hl <- if (highlight) c("prompt, judge, settings score", "tasks in use performance in use",
                         "tuning to the benchmark score") else character(0)
  draw_dag(benchmark_nodes, benchmark_edges, dashed = "tuning to the benchmark capability",
           highlight = hl, boxes = TRUE, text_size = ts, xlim = c(-1.6, 9.6), ylim = c(-0.35, 2.35))
}

fig_chain <- function(ts = 5) {
  ch <- data.frame(name = c("minerals, energy, water", "chips, data centers", "public text and images",
                            "labeling and rating", "model", "product"),
                   x = c(0, 2.1, 1.05, 1.05, 3.7, 4.9), y = c(1.2, 1.2, 0.6, 0, 0.6, 0.6))
  draw_dag(ch, data.frame(from = c("minerals, energy, water", "chips, data centers", "public text and images",
                                   "labeling and rating", "model"),
                          to = c("chips, data centers", "model", "model", "model", "product")),
           boxes = TRUE, text_size = ts, xlim = c(-1.1, 5.4), ylim = c(-0.3, 1.5))
}

fig_hours <- function(ts = 5) {
  d <- data.frame(what = c("all of Wikipedia, 2008", "one year of all work in the UK"),
                  hours = c(1e8, uk_year))
  rng <- data.frame(what = "the text in one training set", lo = words / 1000, hi = words / 100)
  lev <- c("the text in one training set", "one year of all work in the UK", "all of Wikipedia, 2008")
  d$what <- factor(d$what, levels = lev); rng$what <- factor(rng$what, levels = lev)
  ggplot() +
    geom_segment(data = rng, aes(x = lo, xend = hi, y = what, yend = what), color = gold, linewidth = 7,
                 lineend = "butt") +
    geom_point(data = d, aes(hours, what), color = navy, size = 1.2 * ts) +
    scale_x_log10(limits = c(3e7, 3e11), breaks = 10^(8:11),
                  labels = c("100 million", "1 billion", "10 billion", "100 billion")) +
    labs(x = "hours", y = NULL) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
}

fig_bastani <- function(ts = 5) {
  d <- data.frame(phase = c("practice, with the chatbot", "practice, with the chatbot", "exam, chatbot taken away"),
                  arm = c("standard chatbot", "tutor version", "standard chatbot"),
                  pct = c(48, 127, -17))
  d$phase <- factor(d$phase, levels = c("practice, with the chatbot", "exam, chatbot taken away"))
  d$arm <- factor(d$arm, levels = c("standard chatbot", "tutor version"))
  ggplot(d, aes(arm, pct, fill = arm)) +
    geom_hline(yintercept = 0, color = cgray) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%+d%%", pct), vjust = ifelse(pct > 0, -0.4, 1.3)), size = ts) +
    geom_text(data = data.frame(phase = factor("exam, chatbot taken away", levels = levels(d$phase)),
                                arm = factor("tutor version", levels = levels(d$arm)), pct = 0),
              aes(label = "largely\navoided"), vjust = -0.3, size = ts, lineheight = 0.9) +
    facet_wrap(~ phase, scales = "free_x") +
    scale_fill_manual(values = c(navy, gold), guide = "none") +
    scale_y_continuous(limits = c(-35, 145), labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "change from no access") +
    theme(panel.grid.major.x = element_blank())
}

fig_electricity <- function(ts = 5) {
  one <- function(label, share) {
    sq <- expand.grid(col = 0:9, row = 0:9); sq$k <- sq$row * 10 + sq$col
    sq$fill <- pmin(pmax(share - sq$k, 0), 1)
    sq$panel <- label; sq
  }
  sq <- rbind(one("2024: about 1.5%", 1.5), one("2030, base case: just under 3%", 2.9))
  sq$panel <- factor(sq$panel, levels = c("2024: about 1.5%", "2030, base case: just under 3%"))
  ggplot(sq) +
    geom_rect(aes(xmin = col, xmax = col + 0.9, ymin = row, ymax = row + 0.9), fill = "grey90") +
    geom_rect(data = subset(sq, fill > 0), aes(xmin = col, xmax = col + 0.9 * fill, ymin = row, ymax = row + 0.9),
              fill = navy) +
    facet_wrap(~ panel) + coord_fixed() + theme_void() +
    theme(strip.text = element_text(size = 3 * ts, margin = margin(b = 6)))
}

fig_ads <- function(ts = 5) {
  d <- data.frame(sponsor = factor(c("Republican", "Democratic", "other"),
                                   levels = c("other", "Democratic", "Republican")),
                  pct = c(82, 13, 5))
  ggplot(d, aes(x = "ads", y = pct, fill = sponsor)) +
    geom_col(width = 0.9) +
    geom_text(data = subset(d, pct >= 10), aes(label = paste0(sponsor, "\n", pct, "%")),
              position = position_stack(vjust = 0.5), color = "white", size = ts, lineheight = 0.9) +
    coord_flip() +
    scale_fill_manual(values = c(Republican = navy, Democratic = slate, other = cgray), guide = "none") +
    theme_void()
}
