#!/usr/bin/env Rscript
# Run the test suite from the project root:  Rscript scripts/run_tests.R
res <- testthat::test_dir("tests/testthat", reporter = "progress", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\n%d test(s): %d failed, %d errored\n", nrow(df), sum(df$failed), sum(df$error)))
if (sum(df$failed) + sum(df$error) > 0) quit(status = 1)
