test_that("make_expr_weights produce pesos en rango esperado", {
  w <- make_expr_weights(1000, seed = 1)
  expect_true(all(w >= 13))
  expect_true(all(w <= 391))
})
