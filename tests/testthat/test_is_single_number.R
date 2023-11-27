## Test for is_single_number
## by JJAV 20231122

test_that(
  "is_single_number works well",{
    expect_true(is_single_number(3))
    expect_false(is_single_number(c(3,3,3)))
    expect_false(is_single_number(list(a=3)))
    expect_false(is_single_number("3"))
  }
)
