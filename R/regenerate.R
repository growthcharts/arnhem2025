library("dplyr")
library("tidyr")
library("nlreferences")
library("brokenstick")  # v2.2.1
library("AGD")
library("patchwork")
source("R/functions.R")

time  <- readRDS(file = "resources/time") %>%
  dplyr::select(src, id, rec, nrec,
                age, sex, etn, ga, bw, hgt, wgt, hdc)

# remove records with unknown ages
# reduces number of records from 19108 to 19057
# measured on 2151 children
time <- time[!is.na(time$age), ]

# select only term child (ga >= 37) (123 deleted)
# and four observations (106 deleted)
# remains: 1895 children
time <- time %>%
  filter(ga >= 37 & nrec >= 4)

## add Z-scores using current Dutch references
time$bmi <- time$wgt / (time$hgt/100)^2
time <- cbind(time, nlreferences::transform2z(time, verbose = TRUE)) %>% 
  distinct(id, age, .keep_all = TRUE)

# NOTE: The visits for SMOCC occured at
# knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
# there are no data in smocc for 4m (x5) and 7.5m (x7)
ages <- c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)
mids <- ages[-length(ages)] + diff(ages)/2
cutp <- c(-1, mids/12, 3)
time$visit <- cut(time$age, breaks = cutp, labels = ages)

# remove duplicates and select
x <- time %>% 
  select(all_of(c("id", "age", "hgt_z", "wgt_z", "visit")))
x <- split(x, x$id)

# set yname
yname <- "wgt"

z <- c(-0.5, 1.0, 0.5, NA)
t <- c(0, 1, 2, 3)
h <- length(t)
t <- append_future_grid(t, width = 2)
z <- c(z, rep(NA_real_, length(t) - h))
r <- interpolate_cor(t, yname = yname)
zgain_na <- calculate_gain(z, r = r, h = 4)

gains_at_h <- function(data, h, yname = yname) {
  zname <- paste(yname, "z", sep = "_")
  t <- 12 * data[["age"]][1L:h]
  t <- append_future_grid(t, width = 12, delta = 0.5)
  visit <- c(data$visit[1L:h], rep(NA, length(t) - h))
  z <- c(data[[zname]][1L:h], rep(NA, length(t) - h))
  r <- interpolate_cor(t, yname = zname)
  g <- calculate_gain(z, r = r, h = h)
  data.frame(id = rep(data$id[1L], length(t)),
             visit = visit,
             h = h,
             obs = c(rep(TRUE, h), rep(FALSE, length(t) - h)), 
             t = t, z = z, mu = g$hat, sd = sqrt(g$sigma2), 
             gain = g$gain, 
             xmin = c(t[1], lag(t, 1L)[-1]), 
             xmax = t,
             #             ymin = ifelse(g$gain >= 0, 0, g$gain),
             ymin = ifelse(is.na(g$gain) | g$gain >= 0, 0, g$gain),
             #             ymax = ifelse(g$gain >= 0, g$gain, 0),
             ymax = ifelse(is.na(g$gain) | g$gain >= 0, g$gain, 0),
             width = t - c(t[1], lag(t, 1L)[-1]))
}

plot_zscore <- function(data, yname, xlim = c(0, 30), ylim = c(-4, 4),
                        centiles = -2:2) {
  mycolors <- c(grDevices::hcl(120, 100, 40, 0.7),  # green symbol
                grDevices::hcl(240, 100, 40, 0.7),  # blue symbol
                grDevices::hcl(0, 100, 40, 0.7),    # red symbol
                grDevices::hcl(120, 100, 40, 0.8),  # green line
                grDevices::hcl(240, 100, 40, 0.8),  # blue line
                grDevices::hcl(0, 100, 40, 0.8))    # red line
  yname <- match.arg(tolower(yname), c("wgt", "hgt", "dsc"))
  ylab <- switch(yname,
                 wgt = "Gewicht (SDS)",
                 hgt = "HAZ",
                 dsc = "DAZ")
  obs <- filter(data, obs)
  obs$gain <- c(obs$gain[-1L], obs$gain[nrow(obs)])
  exp <- filter(data, !obs)
  n <- nrow(exp)
  prd <- data.frame(
    id = rep(exp$id, length(centiles)),
    x = rep(exp$t, length(centiles)),
    y = rep(exp$mu, length(centiles)) + as.vector(outer(exp$sd, centiles)),
    z = as.factor(rep(centiles, each = n)))
  prd$seq <- c(1:n, (2*n+1):(3*n), (4*n+1):(5*n), (4*n):(3*n+1), (2*n):(n+1))
  g <- ggplot(obs, aes(x = t, y = z, group = id)) + 
    theme_light() + 
    theme(legend.position = c(.85, .15)) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    scale_x_continuous(breaks = seq(0, 30, 3)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = 2, alpha = 0.05,
             fill = mycolors[4]) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    # geom_step(aes(y = gain), lwd = 1.3, col = grDevices::hcl(120, 100, 40, 0.2), 
    #          direction = "hv") +
    # geom_col(aes(y = gain), fill = grDevices::hcl(120, 100, 40, 0.2), 
    #         col = "transparent", width = obs$width, position = position_nudge(x = obs$width/2)) +
    # geom_col(aes(y = gain), fill = grDevices::hcl(120, 100, 40, 0.2), 
    #        col = "transparent", width = obs$width) +
    geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = as.factor(visit)), 
              fill = grDevices::hcl(120, 100, 40, 0.1), lwd = 0, na.rm = TRUE) +
    geom_line(lwd = 0.5, col = mycolors[5]) +
    geom_point(size = 1.5, col = mycolors[2]) +
    xlab("Leeftijd (in maanden)") +
    ylab(ylab) + 
    geom_polygon(data = subset(prd[order(prd$seq), ], seq <= 2 * n), 
                 aes(x = x, y = y, group = id),
                 fill = grDevices::hcl(120, 100, 40, 0.1), colour = "transparent") + 
    geom_polygon(data = subset(prd[order(prd$seq), ], seq > 2 * n & seq <= 4 * n), 
                 aes(x = x, y = y, group = id),
                 fill = grDevices::hcl(120, 100, 40, 0.07), colour = "transparent") + 
    geom_line(data = prd, aes(x = x, y = y, group = z), 
              col = grDevices::hcl(120, 100, 40, 0.2))
  g
}

child_numbers <- c(4, 22, 23, 1, 538)

for (childnum in child_numbers) {
  
  x1 <- x[[childnum]]
  
  # x1 <- x1[-(2:3),]
  x1 <- x1[-5,]
  
  gah <- vector("list", nrow(x1))
  i <- 0L
  for (h in 1:nrow(x1)) {
    i <- i + 1L
    gah[[i]] <- gains_at_h(data = x1, h = h, yname = yname)
  }
  
  graphs <- vector("list", length(gah))
  for (i in 1:length(gah)) {
    graphs[[i]] <- plot_zscore(gah[[i]], yname = yname)
    # print(g)
  }
  
  for (i in 1:length(gah)) {
    fn <- file.path("resources/fig/pdf", paste(paste("child", childnum, i, yname, "dim", sep = "_"), "pdf", sep = "."))
    pdf(fn, width = 8, height = 4.5)
    graph <- graphs[[i]]
    print(graph)
    dev.off()
  }
  
  for (i in 1:length(gah)) {
    fn <- file.path("resources/fig/png", paste(paste("child", childnum, i, yname, "dim", sep = "_"), "png", sep = "."))
    png(fn, width = 3200, height = 1800, res = 288)
    graph <- graphs[[i]]
    print(graph)
    dev.off()
  }
}


