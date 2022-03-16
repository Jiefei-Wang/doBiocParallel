p <- SnowParam(2)
registerDoBiocParallel(p)

test_that("loop with an unnamed argument", {
    res <- foreach(3)%dopar%{
        1
    }
    expect_equal(res, rep(list(1), 3))
})

test_that("loop with an unnamed vector", {
    expect_error(
        foreach(1:3)%dopar%{
            1
        }
    )
})

test_that("loop with a named argument", {
    res <- foreach(i = 1:3)%dopar%{
        i
    }
    expect_equal(res, list(1L, 2L, 3L))
})

test_that("loop with a named argument defined by a variable", {
    x <- 1:3
    res <- foreach(i = x)%dopar%{
        i
    }
    expect_equal(res, list(1L, 2L, 3L))
})

test_that("loop with 2 arguments with the same length", {
    res <- foreach(i = 1:3, j = 2:4)%dopar%{
        c(i, j)
    }
    expect_equal(res, list(1:2, 2:3, 3:4))

})

test_that("loop with 2 arguments with different lengths", {
    expect_warning(
        res <- foreach(i = 1:3, j=2:3)%dopar%{
            c(i,j)
        }
    )
    expect_equal(res, list(1:2, 2:3, 3:2))
})

test_that(".combine", {
    res <- foreach(i = 1:3, .combine = "+")%dopar%{
        i
    }
    expect_equal(res, 6)
})

test_that(".combine + .init", {
    res <- foreach(i = 1:3, .combine = "+", .init = 10)%dopar%{
        i
    }
    expect_equal(res, 16)
})

test_that("loop with error", {
    expect_error(
        foreach(i = 1:3)%dopar%{
            if (i==2)stop()
        }
    )
})

test_that("loop with error, .errorhandling = 'pass'", {
    res <- foreach(i = 1:3, .errorhandling = "pass")%dopar%{
        if (i==2)stop()
        i
    }
    expect_equal(res[[1]], 1)
    expect_true(is(res[[2]], "error"))
    expect_equal(res[[3]], 3)
})

test_that("loop with error, .errorhandling = 'remove'", {
    res <- foreach(i = 1:3, .errorhandling = "remove")%dopar%{
        if (i==2)stop()
        i
    }
    expect_equal(res[[1]], 1)
    expect_equal(res[[2]], 3)
})

test_that("loop with error, .errorhandling = 'remove', .combine = '+'", {
    res <- foreach(i = 1:3, .errorhandling = "remove", .combine = "+")%dopar%{
        if (i==2)stop()
        i
    }
    expect_equal(res, 4)
})

test_that("auto export", {
    x <- 1
    res <- foreach(i = 1:3)%dopar%{
        x
    }
    expect_equal(res, list(1, 1, 1))
})

test_that("no export", {
    x
    expect_error(
        foreach(i = 1:3, .noexport = "x")%dopar%{
            x
        }
    )
})

registerDoSEQ()
gc()
