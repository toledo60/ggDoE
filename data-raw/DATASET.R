library(dplyr)

# Adapted epitaxial layer experiment
data <- read.table("http://www2.isye.gatech.edu/~jeffwu/book/data/adaptedlayer.dat", h=F) # Table 4.1

# Create a function to code -/+ as -1/+1
as.level2=function(x){
  y = rep(0, length(x))
  y[ x=="+" ] = 1
  y[ x=="-" ] = -1
  y
}

design = sapply(data[,2:5], as.level2)
colnames(design) = c("A","B","C","D")

ybar = apply(data[,6:11], 1, mean)
s2 = apply(data[,6:11], 1, var)
lns2 = log(s2)

adapted_epitaxial  = cbind.data.frame(design,ybar, s2, lns2)

adapted_epitaxial <- as_tibble(adapted_epitaxial)


# Original epitaxial layer experiment  -----------------------------------------------------

data <- read.table("https://www2.isye.gatech.edu/~jeffwu/wuhamadabook/data/originallayer.dat", h=F)
#table 4.10


design = sapply(data[,1:4], as.level2)
colnames(design) = c("A", "B", "C", "D")

ybar = apply(data[,5:10], 1, mean)
s2 = apply(data[,5:10], 1, var)
lns2 = log(s2)

original_epitaxial <-  cbind.data.frame(design, ybar, s2, lns2)

original_epitaxial <- as_tibble(original_epitaxial)


# Efficient Designs With Minimal Aliasing Example -------------------------


aliased_design <- tribble(~A, ~B, ~C, ~D, ~E,
                          -1, 1, -1, 1, 1,
                          -1,1,1,1, -1,
                          -1, -1, 1, -1, 1,
                          1, 1, 1, 1, 1,
                          1,1,1, -1, -1,
                          1, -1, -1, 1, -1,
                          -1, 1, -1, -1, 1,
                          1, -1, 1, 1, 1,
                          1, 1, -1, -1, -1,
                          -1, -1, 1, -1, -1,
                          1, -1, -1, -1, 1,
                          -1, -1, -1, 1, -1)




# Reflectance Data, Pulp Experiment: Table 2.1 --------------------------------------------------------

pulp_experiment <- read.table("https://www2.isye.gatech.edu/%7Ejeffwu/book/data/pulp.dat",
                              header = TRUE) |> as_tibble()




# Girder Experiment: Table 3.4 -------------------------------------------------------------


girder_experiment <- read.table("https://www2.isye.gatech.edu/~jeffwu/wuhamadabook/data/girder.dat", h=T) |>
  as_tibble() |>
  mutate(girders = as.factor(1:9)) |>
  tidyr::pivot_longer(cols = 1:4, names_to = 'method', values_to = 'response') |>
  mutate(method = as.factor(method))

g <- lm(response ~ method + girders, data = girder_experiment)
anova(g)





# Bolt Experiment: Table 3.8 --------------------------------------------------------------------

bolt_experiment <- read.table("https://www2.isye.gatech.edu/~jeffwu/wuhamadabook/data/bolt.dat",
                              header=TRUE) |>
  as_tibble()

