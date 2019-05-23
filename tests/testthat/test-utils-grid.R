library(grid)

# A number of convenenience functions used to directly specify units and vps.
# These skip all the tests and reflect current internal structure of matching
# grid objects... This is bad to do but faster in general so I test that the
# output in each case is exactly the same as base grid outptut

# grid units --------------------------------------------------------------

test_that('unit_*() exactly match grid output', {
    expect_equal(unit(1,'npc'), unit_npc(1))
    expect_equal(unit(1,'cm'), unit_cm(1))
    expect_equal(unit(1,'inches'), unit_inches(1))
    expect_equal(unit(1,'lines'), unit_lines(1))
    expect_equal(unit(1,'native'), unit_native(1))
    expect_equal(unit(1,'snpc'), unit_snpc(1))
    expect_equal(unit(1,'mm'), unit_mm(1))
    expect_equal(unit(1,'points'), unit_points(1))
    expect_equal(unit(1:10,'native'), unit_native(1:10))
})

test_that('unit_*() faster than grid::unit()', {
    res <- bench::mark(unit_npc(1), unit(1, 'npc'), relative = T)
    expect_lt(res$median[1], res$median[2])
})


# grid viewport -----------------------------------------------------------

test_that('viewport_*() exactly match grid output', {
    ref1 <- viewport(x = 0.5, y = 0.5, width = 1, height = 1, default.units = 'native', name = 'vp1')
    test1 <- viewport_native(x = 0.5, y = 0.5, width = 1, height = 1, name = 'vp1')
    expect_identical(ref1, test1)
})

test_that('viewport_*() faster than grid::unit()', {
    res <- bench::mark(
        viewport_native(x = 0.5, y = 0.5, width = 1, height = 1, name = 'vp1'),
        viewport(x = 0.5, y = 0.5, width = 1, height = 1, default.units = 'native', name = 'vp1')
    )
    expect_lt(res$median[1], res$median[2])
})
