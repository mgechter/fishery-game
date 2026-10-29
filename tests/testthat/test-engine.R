test_that("asks within the stock are met in full and survivors double up to the cap", {
  r <- resolve_pool_round(stock = 100, asks = rep(5, 6), cap = 100, viable = 6)
  expect_equal(r$catches, rep(5, 6))
  expect_equal(r$harvested, 30)
  expect_equal(r$remaining, 70)
  expect_false(r$collapsed)
  expect_equal(r$stock_next, 100)          # 140 capped at 100
  r2 <- resolve_pool_round(stock = 40, asks = c(10, 10), cap = 100, viable = 2)
  expect_equal(r2$stock_next, 40)          # 20 survivors double
})

test_that("asks beyond the stock are prorated and the pool collapses", {
  r <- resolve_pool_round(stock = 100, asks = rep(20, 6), cap = 100, viable = 6)
  expect_equal(r$total_ask, 120)
  expect_equal(r$catches, rep(100 / 6, 6))
  expect_equal(r$harvested, 100)
  expect_equal(r$remaining, 0)
  expect_true(r$collapsed)
  expect_equal(r$stock_next, 0)
  r2 <- resolve_pool_round(stock = 30, asks = c(20, 10, 0), cap = 100, viable = 3)
  expect_equal(r2$catches, c(20, 10, 0))   # exactly the stock: met in full, then collapse
  expect_true(r2$collapsed)
})

test_that("the viable limit is one fish per player, inclusive", {
  r <- resolve_pool_round(stock = 20, asks = c(5, 5), cap = 20, viable = 2)   # 10 remain
  expect_false(r$collapsed); expect_equal(r$stock_next, 20)
  r <- resolve_pool_round(stock = 20, asks = c(9, 9), cap = 20, viable = 2)   # 2 remain = viable
  expect_false(r$collapsed); expect_equal(r$stock_next, 4)
  r <- resolve_pool_round(stock = 20, asks = c(10, 9), cap = 20, viable = 2)  # 1 remains
  expect_true(r$collapsed); expect_equal(r$stock_next, 0)
})

test_that("an all-zero round leaves the stock to grow", {
  r <- resolve_pool_round(stock = 50, asks = c(0, 0, 0), cap = 100, viable = 3)
  expect_equal(r$catches, c(0, 0, 0)); expect_equal(r$stock_next, 100)
})

test_that("bad asks are refused", {
  expect_error(resolve_pool_round(10, c(1, NA), 10, 2))
  expect_error(resolve_pool_round(10, c(-1, 2), 10, 2))
  expect_error(resolve_pool_round(10, numeric(0), 10, 2))
})

test_that("players are dealt into balanced pools", {
  a <- assign_pools(101:125, 2, seed = 1)
  expect_equal(sort(as.vector(table(a$pool_id))), c(12, 13))
  expect_setequal(a$player_id, 101:125)
  b <- assign_pools(7, 3)                  # one numeric id must not be read as 1:7
  expect_equal(b$player_id, 7); expect_equal(b$pool_id, 1L)
  c1 <- assign_pools(1:10, 4, seed = 2); c2 <- assign_pools(1:10, 4, seed = 2)
  expect_identical(c1, c2)
  expect_equal(max(assign_pools(1:3, 5)$pool_id), 3L)
})

test_that("pool labels read Pool A, Pool B", {
  expect_equal(pool_label(c(1, 2, NA)), c("Pool A", "Pool B", "—"))
})
