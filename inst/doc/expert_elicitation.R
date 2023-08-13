## ----setup, include = F-------------------------------------------------------
knitr::opts_chunk$set(
  echo = T, collapse = T, warning = F, message = F, 
  prompt = T, comment = "#", out.width = "100%"
)

## ---- eval=T, echo=T----------------------------------------------------------
library(tipmap)
set.seed(123)

## ----chips_single1, eval=T, echo=T--------------------------------------------
chips_1exp <- c(1, 3, 4, 2, 0, 0, 0, 0, 0, 0)
sum(chips_1exp)

## ----chips_single2, eval=T, echo=T--------------------------------------------
# Compute cumulative probabilities
(x <- get_cum_probs_1exp(chips_1exp))
# Compute model inputs
(y <- get_model_input_1exp(x))
# Fit beta distribution
(fit_1exp <- fit_beta_1exp(df = y)$par)

## ----chips_multiple, eval=T, echo=T-------------------------------------------
chips_mult <- rbind(
  c(1, 3, 4, 2, 0, 0, 0, 0, 0, 0),
  c(0, 2, 3, 2, 2, 1, 0, 0, 0, 0),
  c(0, 1, 3, 2, 2, 1, 1, 0, 0, 0),
  c(1, 3, 3, 2, 1, 0, 0, 0, 0, 0),
  c(0, 1, 4, 3, 2, 0, 0, 0, 0, 0)
)
beta_fits <- fit_beta_mult_exp(
  chips_mult = chips_mult
)
beta_fits

## ----fit_beta_1a, eval=T, echo=T----------------------------------------------
(alpha <- fit_1exp[1]); (beta <- fit_1exp[2])

# Mean
(beta_mean <- alpha/(alpha+beta))

# Standard deviation
beta_sd <- sqrt( (alpha*beta)/( (alpha+beta)^2 *(alpha+beta+1) ) )
beta_sd

# Mean absolute deviation around the mean
beta_mad_mean <- (2*(alpha^alpha)*(beta^beta))/( beta(alpha, beta) * (alpha+beta)^(alpha+beta+1) )
beta_mad_mean

# Mode
if (alpha > 1 & beta >1) beta_mode <- (alpha-1)/(alpha+beta-2)
if (alpha > 1 & beta >1) beta_mode <- 0.5
beta_mode

# Quantiles
qbeta(p = c(0.001, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99),
      shape1 = alpha, shape2 = beta)

# Samples
x <- rbeta(n = 10^6, shape1 = alpha, shape2 = beta)
mean(x)
sd(x)

## ----fit_beta_2a, eval=T, echo=T----------------------------------------------
expert_samples <- draw_beta_mixture_nsamples(
  n = 10^3, 
  chips_mult = chips_mult
) 
summary(expert_samples)

## ----fit_beta_2b, eval=T, echo=T----------------------------------------------
(mean_w <- round(mean(expert_samples), 2))

## ----load_libs, eval=T, echo=T------------------------------------------------
# Load libraries
packages <- c("magrittr", "ggplot2", "tibble", "dplyr")
invisible(lapply(packages, library, character.only = T))

## ----elicitfig1a, eval=T, echo=T----------------------------------------------
# Create matrix
fits_mat <- as.matrix(beta_fits[,c(1,2)])
# Wide format
fit_beta_mult_plot_wide <- tibble::tibble(
 x = seq(0.001, 0.999, length = 200),
 Expert1 = dbeta(x, fits_mat[1,1], fits_mat[1,2]),
 Expert2 = dbeta(x, fits_mat[2,1], fits_mat[2,2]),
 Expert3 = dbeta(x, fits_mat[3,1], fits_mat[3,2]),
 Expert4 = dbeta(x, fits_mat[4,1], fits_mat[4,2]),
 Expert5 = dbeta(x, fits_mat[5,1], fits_mat[5,2])
)
# Long format
fit_beta_mult_plot_long <- fit_beta_mult_plot_wide %>%
  tidyr::pivot_longer(
    !x,
    names_to = "Expert",
    values_to = "dens")

## ----elicitfig1b, eval=T, echo=T, fig.width=8, fig.height=5, dev=c('png'), out.width="95%"----
# Plot without linear pool
fig_betas_1 <- ggplot(
  data = fit_beta_mult_plot_long,
  aes(x = x, y = dens, goup = Expert)
  ) +
  geom_line(aes(color = Expert)) +
 ggtitle("Fitted beta distributions") +
 xlab("Weight") + ylab("Density") +
 scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) / 10) +
 theme_bw()
print(fig_betas_1)

## ----elicitfig2a, eval=T, echo=T----------------------------------------------
# Wide format
fit_beta_mult_plot_wide2 <- fit_beta_mult_plot_wide %>%
  mutate(linpool = (Expert1 + Expert2 + Expert3 + Expert4 + Expert5)/5)
# Long format
fit_beta_mult_plot_long2 <- fit_beta_mult_plot_wide %>%
  tidyr::pivot_longer(
    !x,
    names_to = "Expert",
    values_to = "dens")

## ----elicitfig2b, eval=T, echo=T, fig.width=8, fig.height=5, dev=c('png'), out.width="95%"----
# Plot
fig_betas_2 <- ggplot(
  data = fit_beta_mult_plot_long2,
  aes(x = x, y = dens, group = Expert)) +
  geom_line(aes(color = Expert ) ) +
  ggtitle("Fitted beta distributions and linear pool") +
  xlab("Weight") + ylab("Density") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)/10) +
  theme_bw() +
  geom_line(data = fit_beta_mult_plot_wide2,
            aes(x = x, y = linpool, group = 1),
            linewidth=1)
print(fig_betas_2)

