# Is the rate of left-handedness different between male and female groups?
# Is left-handedness more likely in the male group?

library(ggplot2)

# create matrix representation of the data
m <- matrix(c(9, 43, 4, 44), 
            nrow = 2,
            byrow = TRUE,
            dimnames = list(sex = c("male", "female"),
                            handedness = c("left", "right")))
# inspect
m

# H_0: The two groups are "not different"
# H_A: The two groups are different

# run two-tailed fisher test: are the two groups different?
fisher.test(m) # p > .05, so H_0

# is left-handedness greater in the male group?
fisher.test(m, alternative = "greater") # p > .05, so H_0


