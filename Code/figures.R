source('Code/helpers.R')
cols <- brewer.pal(3, 'Set1')


#################################################
##### Figure 1: Independent Beta Approach #######
#################################################
pdf('Figures/Figure-1.pdf', width = 10-1/4, height = 9-3/4)
prior <- function(theta1, theta2, a = 1, b = 1) {
  dbeta(theta1, a, b) * dbeta(theta2, a, b)
}

cex.main <- 1.75
cex.axis <- 1.5
cex.lab <- 1.2
col_trans <- rgb(0, 0, 0, alpha = 0.15)
layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
layout(mat = layout.matrix, heights = c(0.25, 2, 2), widths = c(2, 2))

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\theta_1, \\theta_2 \\sim$ Beta(1, 1)'), cex = 2.25)

par(mar = c(5.1, 4.5, 4.1, 2.1))
xx <- seq(0, 1, 0.01)
zz <- outer(
  xx, xx, dprior,
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  what = 'p1p2'
)
zlim <- range(zz)

plot_beta_joint(
  1, length = 500, cex_lab = cex.lab,
  cex.main = cex.main, cex.axis = cex.axis, zlim = zlim
)
mtext(text = expression(theta[2]), side = 2, line = 3, las = 1, cex = cex.lab)

par(mar = c(5.1, 4.5, 3.1, 2.1))
plot_beta_conditional(
  1, ylim = c(0, 1.5),
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis
)
mtext(text = 'Density', side = 2, line = 3, cex = cex.lab)
rect(0, 0, 1, 1, col = adjustcolor(cols[1], 0.50), border = NA)

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\theta_1, \\theta_2 \\sim$ Beta(2, 2)'), cex = 2.25)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot_beta_joint(
  2, length = 500, cex_lab = cex.lab,
  cex.main = cex.main, cex.axis = cex.axis, zlim = zlim
)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot_beta_conditional(
  2, ylim = c(0, 1.5),
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis
)

x <- seq(0, 1, 0.001)
y <- dbeta(x, 2, 2)
polygon(c(x, 1, 0), c(y, 0, 0), col = adjustcolor(cols[1], 0.50), border = NA)
dev.off()


#####################################################
##### Figure 2: Logit Transformation Approach #######
#####################################################
pdf('Figures/Figure-2.pdf', width = 10-1/4, height = 9-3/4)
col_trans <- rgb(0, 0, 0, alpha = 0.15)
layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
layout(mat = layout.matrix, heights = c(0.25, 2, 2), widths = c(2, 2))

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\psi \\sim$ N(0, 1)'), cex = 2.25)

par(mar = c(5.1, 4.5, 4.1, 2.1))
plot_joint_prior(
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  main = 'Joint Prior Distribution',
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis, zlim = zlim
)
mtext(text = expression(theta[2]), side = 2, line = 3, las = 1, cex = cex.lab)

par(mar = c(5.1, 4.5, 4.1, 2.1))
ylim <- c(0, 3.5)
yticks <- seq(0, 3.5, 0.5)

plot_conditional_prior(
  p1 = 0.10, ylim = ylim, yticks = yticks,
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  main = 'Conditional Prior Distribution',
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis
)
mtext(text = 'Density', side = 2, line = 3, cex = cex.lab)

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\psi \\sim$ N(0, 2)'), cex = 2.25)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot_joint_prior(
  prior_par = list(mu_psi = 0, sigma_psi = 2, mu_beta = 0, sigma_beta = 1),
  main = 'Joint Prior Distribution',
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis, zlim = zlim
)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot_conditional_prior(
  p1 = 0.1, ylim = ylim, yticks = yticks,
  prior_par = list(mu_psi = 0, sigma_psi = 2, mu_beta = 0, sigma_beta = 1),
  main = 'Conditional Prior Distribution',
  cex_lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis
)
dev.off()


########################################
##### Figure 3: Prior Comparison #######
########################################
fpsi <- function(y) { exp(y) * (exp(y) * (y - 2) + y + 2) / (exp(y) - 1)^3 }
fbeta <- function(y) { 2 * (exp(2*y) * (exp(2*y) * (2*y - 2) + 2*y + 2)) / (exp(2*y) - 1)^3 }
feta <- function(eta, a = 1) {
  
  Z <- 1 / beta(a, a)
  
  if (eta == 0) {
    return(beta(2 * a - 1, 2 * a - 1) / (beta(a, a)^2))
  }
  
  if (eta > 0) {
    K <- eta^(2 * a - 1) * (1 - eta)^(2 * a - 1) * 
      tolerance::F1(a, 4 * a - 2, 1 - a, 2 * a, 1 - eta, 1 - eta^2)
  } else {
    K <- (-eta)^(2 * a - 1) * (1 + eta)^(2 * a - 1) *
      tolerance::F1(a, 1 - a, 4 * a - 2, 2 * a, 1 - eta^2, 1 + eta)
  }
  
  Z * K
}

x <- seq(-8, 8, 0.01)

get_eta_lt <- function(sigma_psi = 1, samples = 5e5) {
  beta <- rnorm(samples)
  psi <- rnorm(samples, 0, sd = sigma_psi)
  theta1_dep <- exp(beta - psi/2) / (1 + exp(beta - psi/2))
  theta2_dep <- exp(beta + psi/2) / (1 + exp(beta + psi/2))
  eta <- theta2_dep - theta1_dep
  list('eta' = logspline(eta))
}

# Eta
f1_lt <- get_eta_lt(1)
f2_lt <- get_eta_lt(2)
x <- seq(-1, 1, 0.01)
f1_ib <- sapply(x, feta, 1)
f2_ib <- sapply(x, feta, 2)

pdf('Figures/Figure-3.pdf', width = 10-1/4, height = 9-3/4)
layout.matrix <- rbind(c(1, 1, 2, 2), c(3, 4, 4, 3))
layout(mat = layout.matrix, heights = c(2, 2, 2, 2), widths = c(2, 2, 2, 2))

# Eta
plot(
  x, dnorm(x, 0, 1), type = 'l', ylim = c(0, 2.5),
  axes = FALSE, lwd = 0, col = cols[2], xlim = c(-1, 1),
  xlab = expression(eta), ylab = 'Density',
  cex.lab = cex.lab * 1.25, cex.main = cex.main * 1.25,
  cex.axis = cex.axis, main = expression('Prior distributions for' ~ eta)
)

polygon(
  c(x, 1, 0), c(dlogspline(x, f1_lt$eta), 0, 0),
  col = adjustcolor(cols[2], 0.50), border = NA
)

polygon(
  c(x, 1, 0), c(dlogspline(x, f2_lt$eta), 0, 0),
  col = adjustcolor(cols[2], 0.50), border = NA, density = 50
)

polygon(
  c(x, 1, 0), c(f1_ib, 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA
)

polygon(
  c(x, 1, 0), c(f2_ib, 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA, density = 50
)

axis(1, cex.axis = cex.axis, at = seq(-1, 1, 0.5))
axis(2, las = 2, cex.axis = cex.axis)

legend(
  'topleft',
  legend = c(
    expression(sigma[psi] ~ ' = 1'),
    expression(sigma[psi] ~ ' = 2'),
    expression('a = 1'),
    expression('a = 2')
  ),
  fill = rep(cols[2:1], each = 2), bty = 'n', cex = 1.5,
  density = c(100, 50, 100, 50),
  y.intersp = 1.25
)


# Psi
samples <- 1e5
psi1 <- rlogis(samples) - rlogis(samples)
fpsi1_dens <- logspline(psi1)

psi2 <- rlogis(samples, log(2), 1/2) - rlogis(samples, log(2), 1/2)
fpsi2_dens <- logspline(psi2)

x <- seq(-8, 8, 0.01)

plot(
  x, dnorm(x, 0, 1), type = 'l',
  axes = FALSE, lwd = 0, col = cols[2],
  xlab = expression(psi), ylab = 'Density',
  cex.lab = cex.lab * 1.25, cex.main = cex.main * 1.25,
  cex.axis = cex.axis, main = expression('Prior distributions for' ~ psi)
)

polygon(
  c(x, 1, 0), c(dnorm(x, 0, 1), 0, 0), col = adjustcolor(cols[2], 0.50), border = NA
)

polygon(
  c(x, 1, 0), c(dnorm(x, 0, 2), 0, 0),
  col = adjustcolor(cols[2], 0.50), border = NA, density = 50
)

y1fpsi <- fpsi(x[x != 0])
y2fpsi <- dlogspline(x, fpsi2_dens)

polygon(
  c(x[x != 0], 1, 0), c(y1fpsi, 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA
)

polygon(
  c(x, 1, 0), c(y2fpsi, 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA, density = 50
)

axis(1, cex.axis = cex.axis, at = seq(-8, 8, 4))
axis(2, las = 2, cex.axis = cex.axis)

legend(
  'topleft',
  legend = c(
    expression(sigma[psi] ~ ' = 1'),
    expression(sigma[psi] ~ ' = 2'),
    expression('a = 1'),
    expression('a = 2')
  ),
  fill = rep(cols[2:1], each = 2), bty = 'n', cex = 1.5,
  density = c(100, 50, 100, 50),
  y.intersp = 1.25
)


# Thetas
plot(0, 0, cex = 0, axes = FALSE, xlab = '', ylab = '')

par(mar = c(5.1, 4.5, 2.1, 2.1))
theta <- seq(0.000001, 0.999999, 0.01)
plot(
  theta, dbeta(theta, 0, 1), type = 'l',
  axes = FALSE, lwd = 0, col = cols[2],
  xlab = expression(theta[1] ~ ',' ~ theta[2]), ylab = 'Density',
  cex.lab = cex.lab * 1.25, cex.main = cex.main * 1.25, xlim = c(0, 1), ylim = c(0, 2),
  main = expression('Prior distributions for' ~ theta[1] ~ ' and' ~ theta[2])
)

dp2 <- function(p2, beta_psi = 1, sigma_psi = 1) {
  
  res <- integrate(function(p1) {
    J <- 1 / (p1 * p2 * (1 - p1) * (1 - p2))
    beta <- 0.50 * (log(p1 / (1 - p1)) + log(p2 / (1 - p2)))
    psi <- log(p2 / (1 - p2)) - log(p1 / (1 - p1))
    
    J * dnorm(beta, 0, beta_psi) * dnorm(psi, 0, sigma_psi)
  }, lower = 0, upper = 1)$value
  
  res
}

y1 <- sapply(theta, dp2)
y2 <- sapply(theta, dp2, sigma_psi = 2)
polygon(
  c(theta, 1, 0), c(y1, 0, 0), col = adjustcolor(cols[2], 0.50), border = NA
)

polygon(
  c(theta, 1, 0), c(dbeta(theta, 1, 1), 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA
)

polygon(
  c(theta, 1, 0), c(y2, 0, 0),
  col = adjustcolor(cols[2], 0.50), border = NA, density = 50
)

polygon(
  c(theta, 1, 0), c(dbeta(theta, 2, 2), 0, 0),
  col = adjustcolor(cols[1], 0.50), border = NA, density = 50
)
axis(1, cex.axis = cex.axis)
axis(2, las = 2, cex.axis = cex.axis)
legend(
  'topleft',
  legend = c(
    expression(sigma[psi] ~ ' = 1'),
    expression(sigma[psi] ~ ' = 2'),
    expression('a = 1'),
    expression('a = 2')
  ),
  fill = rep(cols[2:1], each = 2), bty = 'n', cex = 1.5,
  density = c(100, 50, 100, 50),
  y.intersp = 1.25
)
dev.off()


##########################################
###### Figure 4: Reanalysis Figure ####### 
##########################################
dat <- read.csv('Data/NEJM.csv')

bfindep_dat <- sapply(seq(1, 5, length.out = 100), function(a) {
  get_bfindep_anal(dat$y1, dat$y2, dat$n1, dat$n2, a, a)
})

bfdep_dat <- sapply(seq(1, 2, length.out = 100), function(sigma) {
  apply(dat, 1, function(x) (get_bfab(x[4], x[5], x[6], x[7], sigma_psi = sigma)))
})

n <- 100
ys <- seq(0, 50, 1)
bfind <- sapply(seq(1, 5, length.out = 100), function(a) {
  sapply(ys, function(y) { get_bfindep_anal(y, y, n, n, a, a) })
})

bfdep <- sapply(seq(1, 2, length.out = 100), function(sigma_psi) {
  sapply(ys, function(y) { get_bfab(y, y, n, n, sigma_psi = sigma_psi, sigma_beta = 1) })
})

alphas_indep <- c(1, seq(0.05, 0.010, length.out = 99))
cols_indep <- sapply(alphas_indep, function(alpha) {
  adjustcolor(cols[1], alpha.f = alpha)
})

alphas_dep <- c(1, seq(0.05, 0.010, length.out = 99))
cols_dep <- sapply(alphas_dep, function(alpha) {
  adjustcolor(cols[2], alpha.f = alpha)
})

plot_bfs <- function(bfind, bfdep, ylim) {
  n <- 100
  ys <- seq(0, 50, 1)
  
  plot(
    ys / n, exp(-bfind[, 1]), col = cols[1], lwd = 2,
    xlab = '', ylab = '', log = 'y',
    main = TeX('Bayes factors under $H_0$'),
    font.main = 1, type = 'l', ylim = ylim, axes = FALSE, 
    cex.main = cex.main, cex.lab = cex.lab
  )
  
  lines(ys / n, exp(-(bfind[, 1])), col = cols[1], lwd = 2)
  lines(ys / n, exp(-(bfdep[, 1])), col = cols[2], lwd = 2)
  
  for (i in seq(2, 100)) {
    lines(ys / n, exp(-(bfind[, i])), col = cols_indep[i], lwd = 2)
  }
  
  for (i in seq(2, 100)) {
    lines(ys / n, exp(-(bfdep[, i])), col = cols_dep[i], lwd = 2)
  }
  
  axis(1, cex.axis = 1.25)
  axis(2, las = 2, cex.axis = 1.25)
  
  mtext(
    TeX('$\\frac{y}{100}$'), side = 1,
    line = 3.5, cex = cex.lab
  )
}

pdf('Figures/Figure-4.pdf', width = 10-1/4, height = 9-3/4)
cex.axis <- 1.25
layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
layout(mat = layout.matrix, heights = c(2, 2), widths = c(2, 2))

par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(
  seq(1, 39), exp(-bfindep_dat[, 1]), pch = 20, axes = FALSE, cex = 1.5, log = 'y',
  xlab = 'Study ID', ylab = '', ylim = c(1, 200), col = cols[1],
  main = 'Bayes factors across studies', font.main = 1,
  cex.main = cex.main, cex.lab = cex.lab
)
axis(1, cex.axis = cex.axis, at = c(1, seq(5, 35, 5), 39))
axis(2, las = 2, cex.axis = cex.axis, at = c(1, 10, 100, 200))

for (i in seq(2, 100)) {
  points(
    exp(-bfindep_dat[, i]), pch = 20, cex = 1.5,
    col = cols_indep[i]
  )
}

for (i in seq(1, 100)) {
  points(
    exp(-bfdep_dat[, i]), pch = 20, cex = 1.5,
    col = cols_dep[i]
  )
}

mtext(text = TeX('$BF_{01}$'), side = 2, line = 2.5, cex = cex.lab)

points_x <- seq(33, 37.5, length.out = 100)
points_y <- rep(200, 100)
off <- 65

cexx <- 1
text(26.5, 200, 'IB Approach', cex = cexx)
points(points_x, points_y, pch = 20, cex = 2, col = cols_indep)
points(points_x, points_y - off, pch = 20, cex = 2, col = cols_dep)
text(26.5, 200 - off, 'LT Approach', cex = cexx)

text(31.75, 200, '1', cex = cexx)
text(38.75, 200, '5', cex = cexx)
text(31.75, 200 - off, '1', cex = cexx)
text(38.75, 200 - off, '2', cex = cexx)

par(mar = c(5.1, 4.5, 3.1, 2.1))
a <- 1
plot_beta_joint(
  a, length = 500, cex_lab = 1.5, xlim = c(0, 0.5), ylim = c(0, 0.5),
  cex.main = cex.main, cex.axis = cex.axis, zlim = zlim,
  main = TeX('$\\theta_1, \\theta_2 \\sim$ Beta(1, 1)')
)
mtext(text = expression(theta[2]), side = 2, line = 2.5, las = 1, cex = 1.5)
abline(a = 0, b = 1, col = 'gray66', lty = 1)

points_col <- rep('black', 39)
points_col[seq(12)] <- cols[2]
points(dat$y1n1, dat$y2n2, pch = 20, col = points_col)

offset_x <- rep(0.0095, 39)
offset_x[seq(9)] <- offset_x[10] * 3/4
offset_y <- rep(0.001, 39)

offset_x[1] <- offset_x[1] * 2 / 4
offset_y[1] <- offset_y[1] + 0.01
offset_x[5] <- 0
offset_y[5] <- 0.0125

offset_x[6] <- offset_x[6] * 3 / 4
offset_y[6] <- offset_y[6] * 8

offset_x[8] <- -offset_x[8]
offset_y[3] <- -offset_y[3]

offset_x[12] <- offset_x[12] * 1.2

offset_x[19] <- -offset_x[19]
offset_y[19] <- offset_y[19] * 2

offset_y[14] <- offset_y[14] * 3
offset_y[37] <- -offset_y[37] * 3

points(dat$y1n1, dat$y2n2, pch = 20, col = points_col)
text(dat$y1n1 + offset_x, dat$y2n2 + offset_y, seq(39), cex = 0.5, col = points_col)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot_bfs(bfind, bfdep, c(1, 100))

par(mar = c(5.1, 4.1, 3.1, 2.1))
plot_joint_prior(
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  main = TeX('$\\psi \\sim$ N(0, 1)'), xlim = c(0, 0.5), ylim = c(0, 0.5),
  cex_lab = 1.5, cex.main = cex.main, cex.axis = cex.axis, zlim = zlim
)
abline(a = 0, b = 1, col = 'gray66', lty = 1)
points(dat$y1n1, dat$y2n2, pch = 20, col = points_col)
text(dat$y1n1 + offset_x, dat$y2n2 + offset_y, seq(39), cex = 0.5, col = points_col)
dev.off()


#########################################################
##### Figure 5: Expected Rate / Log Odds Difference #####
#########################################################
n <- 5e5
ys <- seq(0, 100)
logit <- function(x) log(x / (1 - x))
get_logodds_diff <- function(theta1, theta2) {
  logit(theta2) - logit(theta1)
}

priors <- abtest::simulate_priors(n)
res <- MASS::kde2d(c(0, priors$p1, 1), c(-1, priors$arisk, 1), n = 100)
res_logodds <- MASS::kde2d(c(0, priors$p1, 1), c(-6, priors$logor, 6), n = 100)

priors2 <- cbind(rbeta(n, 1, 1), rbeta(n, 1, 1))
res2 <- MASS::kde2d(priors2[, 1], priors2[, 2] - priors2[, 1], n = 100)
res2_logodds <- MASS::kde2d(
  c(0, priors2[, 1], 1), c(-6, get_logodds_diff(priors2[, 1], priors2[, 2]), 6), n = 100
)

pdf('Figures/Figure-5.pdf', width = 10-1/4, height = 9-3/4)
xx <- seq(0, 1, 0.01)
zz <- outer(
  xx, xx, dprior,
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  what = 'p1p2'
)
zlim <- range(zz)

yy <- seq(-4, 4, 0.01)
zz2 <- outer(
  yy, yy, dprior,
  prior_par = list(mu_psi = 0, sigma_psi = 1/2, mu_beta = 0, sigma_beta = 1),
  what = 'logor'
)
zlim2 <- range(zz2) * 0.85

layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
layout(mat = layout.matrix, heights = c(0.25, 2, 2), widths = c(2, 2))

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\theta_1, \\theta_2 \\sim$ Beta(1, 1)'), cex = 2.25)

par(mar = c(5.1, 4.5, 4.1, 2.1))
## Rate scale IB
image(
  res2$x, res2$y, res2$z, xlab = '', ylab = '', las = 1, zlim = zlim * 1.1,
  col = hcl.colors(10, 'YlOrRd', rev = TRUE), cex.main = cex.main, cex.axis = cex.axis,
  main = 'Expected Rate Difference', xlim = c(0, 1), ylim = c(-1, 1), font.main = 1
)

mtext(text = expression(theta[1]), side = 1, line = 2.8, cex = 1.5)
mtext(text = expression(eta), side = 2, line = 2.5, cex = 1.5)

par(mar = c(5.1, 4.5, 4.1, 2.1))
## Log odds scale IB
image(
  res2_logodds$x, res2_logodds$y, res2_logodds$z, xlab = '', ylab = '', las = 1, zlim = zlim2,
  col = hcl.colors(10, 'YlOrRd', rev = TRUE), cex.main = cex.main, cex.axis = cex.axis,
  main = 'Expected Log Odds Difference', xlim = c(0, 1), ylim = c(-6, 6), font.main = 1
)
mtext(text = expression(theta[1]), side = 1, line = 2.8, cex = 1.5)
mtext(text = expression(psi), side = 2, line = 2.5, cex = 1.5)

par(mar = c(0, 3, 0, 0))
plot(0, 0, axes = FALSE, cex = 0, xlab = '', ylab = '')
text(0, 0, TeX('$\\psi \\sim$ N(0, 1)'), cex = 2.25)

par(mar = c(5.1, 4.1, 4.1, 2.1))
## Rate scale LT
image(
  res$x, res$y, res$z, xlab = '', ylab = '', las = 1, zlim = zlim * 1.1, # zlim = zlim,
  col = hcl.colors(10, 'YlOrRd', rev = TRUE), cex.main = cex.main, cex.axis = cex.axis,
  main = 'Expected Rate Difference', xlim = c(0, 1), ylim = c(-1, 1), font.main = 1
)
mtext(text = expression(theta[1]), side = 1, line = 2.8, cex = 1.5)

par(mar = c(5.1, 4.1, 4.1, 2.1))
## Log odds scale LT
image(
  res_logodds$x, res_logodds$y, res_logodds$z, xlab = '', ylab = '', las = 1, zlim = zlim2,
  col = hcl.colors(10, 'YlOrRd', rev = TRUE), cex.main = cex.main, cex.axis = cex.axis,
  main = 'Expected Log Odds Difference', xlim = c(0, 1), ylim = c(-6, 6), font.main = 1
)
mtext(text = expression(theta[1]), side = 1, line = 2.8, cex = 1.5)
dev.off()


#######################################################
##### Figuree 6: Posterior Summary across Studies #####
#######################################################
post <- apply(dat, 1, function(x) {
  get_post(x[4], x[5], x[6], x[7], a = 1, b = 1, sigma_psi = 1, samples = 8e5)
})

plot_posterior <- function(post, type = 'psi') {
  ids <- seq(1, 39)
  
  if (type == 'psi') {
    main <- expression('Posterior summary of ' ~ psi ~ ' across studies')
    indices <- c(10, 9, 3, 4, 1, 2)
    ylim <- c(-2, 2)
    xlab <- ''
    
  } else {
    main <- expression('Posterior summary of ' ~ eta ~ ' across studies')
    indices <- c(12, 11, 5, 6, 7, 8)
    ylim <- c(-0.30, 0.30)
    xlab <- 'Study ID'
    
  }
  
  plot(
    ids, post[indices[1], ], pch = 20, axes = FALSE, cex = 1.5,
    xlab = xlab, ylab = '', col = cols[1], ylim = ylim,
    main = main, font.main = 1, cex.main = cex.main, cex.lab = cex.lab
  )
  
  lines(c(1, 39), c(0, 0), lty = 2, col = 'gray76')
  points(ids, post[indices[1], ], pch = 20, cex = 1.5, col = cols[1])
  
  arrows(
    x0 = ids, x1 = ids, y0 = post[indices[3], ], y1 = post[indices[4], ],
    col = cols[1], length = 0
  )
  
  ids_off <- ids + 0.425
  
  points(
    ids_off, post[indices[2], ], pch = 20, cex = 1.5, col = cols[2]
  )
  
  arrows(
    x0 = ids_off, x1 = ids_off, y0 = post[indices[5], ], y1 = post[indices[6], ],
    col = cols[2], length = 0
  )
  
  axis(1, cex.axis = cex.axis, at = c(1, seq(5, 35, 5), 39))
  axis(2, las = 2, cex.axis = cex.axis)
}

pdf('Figures/Figure-6.pdf', width = 10-1/4, height = 10-3/4)
par(mfrow = c(2, 1))
par(mar = c(3.1, 4.5, 4.1, 2.1))

plot_posterior(post, type = 'psi')
mtext(
  expression(psi), side = 2,
  line = 2.65, cex = 1.5
)

legend(
  'topright',
  legend = c('IB Approach', 'LT Approach'),
  col = c(cols[1], cols[2]), pch = 20, bty = 'n', cex = 1,
  y.intersp = 1.25, lty = 1
)

par(mar = c(5.1, 4.5, 2.1, 2.1))
plot_posterior(post, type = 'eta')
mtext(
  expression(eta), side = 2,
  line = 2.65, cex = 1.5
)
dev.off()


#####################################
##### Figure 7: Linear Coupling #####
#####################################
ms <- '
model {
  eta ~ dnorm(0, pow(sigma_eta, -2))T(-1, 1)
  zeta ~ dnorm(0.50, pow(sigma_zeta, -2))T(0, 1)
  
  theta1 = min(max(zeta - eta/2, 0), 1)
  theta2 = min(max(zeta + eta/2, 0), 1)
  
  y1 ~ dbin(theta1, n1)
  y2 ~ dbin(theta2, n2)
}'


run_model <- function(y1, y2, n1, n2, sigma_eta, sigma_zeta) {
  dat <- list(
    'sigma_eta' = sigma_eta,
    'sigma_zeta' = sigma_zeta,
    'y1' = y1,
    'y2' = y2,
    'n1' = n1,
    'n2' = n2
  )
  
  params <- c('eta', 'zeta', 'theta1', 'theta2')
  model <- jags.model(textConnection(ms), dat = dat, n.chains = 6, quiet = TRUE)
  samples <- coda.samples(model, variable.names = params, n.iter = 6000)
  samples <- do.call('rbind', samples)
  
  deta <- logspline(samples[, 1])
  bf01 <- dlogspline(0, deta) / dtruncnorm(0, -1, 1, 0, dat$sigma_eta) # BF01
  list(
    'bf01' = bf01,
    'samples' = samples,
    'correlation' = cor(samples[, 2], samples[, 3])
  )
}

n <- 100
ys <- seq(1, 50)
res <- run_model(NA, NA, n, n, sigma_eta = 0.20, sigma_zeta = 0.50)
priors <- res$samples
kd <- MASS::kde2d(priors[, 2], priors[, 3], n = 100)

if (!file.exists('Data/linear-coupling-bfs.RDS')) {
  registerDoParallel(cores = 8)
  ys <- seq(1, 50)
  bfs <- foreach(i = seq(0.20, 1, length.out = 100)) %do% {
    sapply(ys, function(y) { run_model(y, y, 100, 100, sigma_eta = i, sigma_zeta = 0.50)$bf01 })
  }
  bfs_dat <- do.call('cbind', bfs)
  saveRDS(bfs_dat, 'Data/linear-coupling-bfs.RDS')
  
} else {
  bfs_dat <- readRDS('Data/linear-coupling-bfs.RDS')
}

xx <- seq(0, 1, 0.01)
zz <- outer(
  xx, xx, dprior,
  prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1),
  what = 'p1p2'
)
zlim <- range(zz)

## Rate scale
cex.main <- 1.75
cex.axis <- 1.25
cex.lab <- 1.2

pdf('Figures/Figure-7.pdf', width = 10-1/4, height = 6-3/4)
par(mfrow = c(1, 2))
image(
  kd$x, kd$y, kd$z, xlab = '', ylab = '', las = 1, zlim = zlim * 1.02, # zlim = zlim,
  col = hcl.colors(10, 'YlOrRd', rev = TRUE), cex.main = cex.main, cex.axis = cex.axis,
  main = 'Joint Prior Distribution', xlim = c(0, 1), ylim = c(0, 1), font.main = 1
)
mtext(text = expression(theta[1]), side = 1, line = 2.8, cex = 1.5)
mtext(text = expression(theta[2]), side = 2, line = 2.8, cex = 1.5)

alphas_indep <- seq(0.02, 0.06, length.out = 99)
cols_indep <- sapply(alphas_indep, function(alpha) {
  adjustcolor(cols[1], alpha.f = alpha)
})

plot(
  ys / n, bfs_dat[, 100], col = cols[1], lwd = 2,
  xlab = '', ylab = '', log = 'y',
  main = TeX('Bayes factors under $H_0$'),
  font.main = 1, type = 'l', ylim = c(1, 100), axes = FALSE, 
  cex.main = cex.main, cex.lab = cex.lab
)

for (i in seq(1, 99)) {
  lines(ys / n, bfs_dat[, i], col = cols_indep[i], lwd = 2)
}

axis(1, cex.axis = 1.25)
axis(2, las = 2, cex.axis = 1.25)

mtext(
  TeX('$\\frac{y}{100}$'), side = 1,
  line = 3.5, cex = cex.lab
)

mtext(
  TeX('$BF_{01}$'), side = 2,
  line = 2.5, cex = cex.lab
)

scaling <- 20
y <- 3.8 * scaling
points_x <- seq(0.35, 0.45, length.out = 100)
points_y <- rep(y, 100)

points(points_x, points_y, pch = 20, cex = 2, col = c(cols_indep, cols[1]))
text(0.32, y, '0.20', cex = 0.75)
text(0.47, y, '1', cex = 0.75)
dev.off()
