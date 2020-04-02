context("y-structure")
library(collidR)

test_that("simulation", {
	a <- simulate_y_structure(n=50000000, prop=0.02, or_sz=3, b_xy=0, rsq_xs=0.2, rsq_ys=0.2, vx=1, vy=1, vs=1, sig.level=5e-8)
	expect_true(all(!is.na(unlist(a))))
	b <- simulate_y_structure(n=NA, prop=0.02, or_sz=3, b_xy=0, rsq_xs=0.2, rsq_ys=0.2, vx=1, vy=1, vs=1, sig.level=5e-8)
	expect_true(is.na(b$pow) & !is.na(b$bh_xy))
})


test_that("plot1", {
	a <- plot_simulate_y_structure(0.02, -0.03)
	expect_true("ggplot" %in% class(a))
})

test_that("plot2", {
	a <- plot_simulate_y_structure_optim(0.02, -0.03)
	expect_true("ggplot" %in% class(a))
})

