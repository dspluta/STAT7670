# https://bookdown.org/sz_psyc490/r4psychometics/factor-analysis.html

scores <- read.table("https://raw.githubusercontent.com/sunbeomk/PSYC490/main/scores.txt")
head(scores)

cor_subtests <- cor(scores)

round(cor_subtests, 2)
heatmap(round(cor_subtests, 2))

#####
# EFA with 1 factor

efa_1 <- factanal(x = scores, factors = 1)

efa_1

####
# EFA with 2 factors

efa_2 <- factanal(x = scores, factors = 2)

efa_2

###

efa_3 <- factanal(x = scores, factors = 3)

efa_3

# 