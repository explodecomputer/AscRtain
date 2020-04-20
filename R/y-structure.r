#' Y-structure collider
#'
#' X and Y are continuous, they influence continuous S which predicts binary inclusion variable Z
#'
#' @param n Population sample size
#' @param prop Proportion of population in sample
#' @param or_sz Odds ratio for the effect of collider variable S on being included
#' @param b_xy Causal effect of X on Y
#' @param rsq_xs Variance explained by X on collider variable S
#' @param rsq_ys Variance explained by Y on collider variable S
#' @param vx Variance of X. Default = `1`.
#' @param vy Variance of Y. Default = `1`.
#' @param vs Variance of S. Default = `1`.
#' @param sig.level Alpha threshold for power calculation. Default = `5e-8`
#'
#' @export
#' @return List of simulation results
simulate_y_structure <- function(n, prop, or_sz, b_xy, rsq_xs, rsq_ys, vx=1, vy=1, vs=1, sig.level=5e-8)
{
  # infer
  logor_sz <- log(or_sz)
  vz <- prop * (1-prop)
  b_xs <- sqrt(abs(rsq_xs) * vs / (vx)) * sign(rsq_xs)
  b_ys <- sqrt(abs(rsq_ys) * vs / vy) * sign(rsq_ys)
  ve <- vs - b_xs^2 * vx - b_ys^2 * vy
  alpha <- log((1 - prop)/prop)
  bh_xy <- (b_xy * vz * vx - b_xs * logor_sz * vz * b_ys * logor_sz * vz * vx * vy) / ((b_ys * logor_sz * vz)^2 * vx * vy + vx*vz)
  cor_xy <- sqrt(bh_xy^2 * vx / vy) * sign(bh_xy)
  if(is.na(n))
  {
    return(list(bh_xy=bh_xy, cor_xy=cor_xy, pow=NA, se=NA, pval=NA, ns=NA))
  }
  ns <- n * prop
  if(ns == 0)
  {
    pow <- 0
  } else {
    pow <- power.anova.test(groups=2, n=ns, between.var=cor_xy^2, within.var = 1 - cor_xy^2, sig.level= sig.level)$power
  }
  se <- sqrt((1 - cor_xy^2) * vs / (ns * vx))
  pval <- pnorm(abs(bh_xy / se), lower.tail=FALSE)
  return(list(bh_xy=bh_xy, cor_xy=cor_xy, pow=pow, se=se, pval=pval, ns=ns))
}

#' Plot Y structure bias across range of parameters
#'
#' Takes a range of OR_sz values, and ranges of rsq_ys and rsq_xs values. Then, for a given proportion of the population being sampled, provides the expected X-Y association for each of the parameter combinations. Enumerates over all combinations so can be slow 
#'
#' @param prop Proportion of population included in sample
#' @param b_xy_thresh Target b_xy association - e.g. what value of b_xy are you suspicious could be due to ascertainment on a collider
#' @param or_sz_range Range of OR_sz values to enumerate over. Default = `c(1,100)`
#' @param b_xy Suspected true X-Y effect. Default = `0`
#' @param rsq_xs_range Range of rsq_xs values to enumerate over. Default = `c(0,1)`
#' @param rsq_ys_range  Range of rsq_ys values to enumerate over. Default = `c(0,1)`
#' @param gran Granularity of ranges. Default = `30`
#'
#' @export
#' @return ggplot of simulations
plot_simulate_y_structure <- function(prop, b_xy_thresh, or_sz_range=c(1,100), b_xy=0, rsq_xs_range=c(0,1), rsq_ys_range=c(0,1), gran=30)
{
  message("Calculating surface")
  param <- expand.grid(
      or_sz = exp(seq(log(or_sz_range[1]), log(or_sz_range[2]), length.out=gran)),
      rsq_xs=seq(rsq_xs_range[1], rsq_xs_range[2], length.out=gran),
      rsq_ys=seq(rsq_ys_range[1], rsq_ys_range[2], length.out=gran),
      bh_xy=NA
    ) %>% dplyr::filter(rsq_xs + rsq_ys <=1)
  for(i in 1:nrow(param))
  {
    o <- simulate_y_structure(NA, prop, param$or_sz[i], b_xy, param$rsq_xs[i], param$rsq_ys[i])
    param$bh_xy[i] <- o$bh_xy
  }
    param <- dplyr::mutate(param, thresh = abs(bh_xy) >= abs(b_xy_thresh) & sign(bh_xy) == sign(b_xy_thresh))
    param2 <- param %>% dplyr::filter(thresh) %>% 
      dplyr::group_by(rsq_xs, rsq_ys) %>%
      dplyr::summarise(or_sz = min(or_sz))

  message("Plotting")
  g <- ggplot2::ggplot(param, ggplot2::aes(x=rsq_xs, y=rsq_ys)) +
    ggplot2::geom_point(colour="grey", size=0.2) +
    ggplot2::geom_point(data=param2, ggplot2::aes(colour=or_sz), size=2) +
    ggplot2::scale_colour_distiller(palette = "Spectral") +
    ggplot2::labs(x="Variance in S explained by X", y="Variance in S explained by Y", colour="OR (S->Z)")
  g
}

#' Plot the minimum Y-structure required to explain some association
#'
#' For a given Y-structure, can an association of b_xy can be obtained through ascertainment on a collider? Use optimisation to determine the minimum OR_sz for a range of rsq_xs and rsq_ys values
#'
#' @param prop Proportion of population included in sample
#' @param b_xy_thresh Target b_xy association - e.g. what value of b_xy are you suspicious could be due to ascertainment on a collider
#' @param b_xy Suspected true X-Y effect. Default = `0`
#' @param rsq_xs_range Range of rsq_xs values to enumerate over. Default = `c(0,1)`
#' @param rsq_ys_range  Range of rsq_ys values to enumerate over. Default = `c(0,1)`
#' @param gran Granularity of ranges. Default = `101`
#' @param max_or_sz Maximum OR_sz to allow in optimisation.
#'
#' @export
#' @return ggplot of simulations
plot_simulate_y_structure_optim <- function(prop, b_xy_thresh, b_xy=0, rsq_xs_range=c(0,1), rsq_ys_range=c(0,1), gran=101, max_or_sz=20)
{
  message("Calculating surface")
  param <- expand.grid(
      rsq_xs=seq(rsq_xs_range[1], rsq_xs_range[2], length.out=gran),
      rsq_ys=seq(rsq_ys_range[1], rsq_ys_range[2], length.out=gran),
      bh_xy=NA,
      or_sz=NA
    ) %>% dplyr::filter(rsq_xs + rsq_ys <=1)
  param$id <- 1:nrow(param)
  fn <- function(x, prop, b_xy, rsq_xs, rsq_ys, b_xy_thresh) {
    o <- simulate_y_structure(NA, prop, x, b_xy, rsq_xs, rsq_ys)
    return((o$bh_xy - b_xy_thresh)^2)
  }
  for(i in 1:nrow(param))
  {
    o <- optimize(fn, c(1,max_or_sz+1), prop=prop, b_xy=b_xy, rsq_xs=param$rsq_xs[i], rsq_ys=param$rsq_ys[i], b_xy_thresh=b_xy_thresh)
    param$bh_xy[i] <- b_xy_thresh
    param$or_sz[i] <- o$minimum
  }
  param$thresh <- param$or_sz <= max_or_sz
    
  message("Plotting")
  g <- ggplot2::ggplot(param %>% subset(., !thresh), ggplot2::aes(x=rsq_xs, y=rsq_ys)) +
    ggplot2::geom_point(colour="grey", size=0.2) +
    ggplot2::geom_point(data=param %>% subset(., thresh), ggplot2::aes(colour=or_sz), size=2) +
    ggplot2::scale_colour_distiller(palette = "Spectral") +
    ggplot2::labs(x="Variance in S explained by X", y="Variance in S explained by Y", colour="OR of S\non inclusion")
  g
}
