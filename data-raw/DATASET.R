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




# Throughput --------------------------------------------------------------


throughput_dat <- tribble( ~Day, ~ Operator, ~Machine, ~Method, ~Throughput,
                       1, 		 1, 	  	 "alpha" ,	  	 "A", 	  	 102,
                       2, 	  	 1, 	  	 "gamma", 	  	 "B", 	  	 92,
                       3, 	  	 1, 	  	 "epsilon", 	 "C", 	  	 96,
                       4, 	  	 1, 	  	 "beta", 	  	 "D", 	  	 120,
                       5, 	  	 1, 	  	 "delta", 	  	 "E", 	  	 123,
                       1, 	  	 2, 	  	 "beta", 	  	 "B", 	  	 105,
                       2, 	  	 2, 	  	 "delta", 	  	 "C", 	  	 112,
                       3, 	  	 2, 	  	 "alpha", 	  	 "D", 	  	 130,
                       4, 	  	 2, 	  	 "gamma", 	  	 "E", 	  	 100,
                       5, 	  	 2, 	  	 "epsilon", 	 "A", 	  	 110,
                       1, 	  	 3, 	  	 "gamma", 	  	 "C", 	  	 82,
                       2, 	  	 3, 	  	 "epsilon", 	 "D", 	  	 131,
                       3, 	  	 3, 	  	 "beta", 	  	 "E", 	  	 108,
                       4, 	  	 3, 	  	 "delta", 	  	 "A", 	  	 111,
                       5, 	  	 3, 	  	 "alpha", 	  	 "B", 	  	 111,
                       1, 	  	 4, 	  	 "delta", 	  	 "D", 	  	 141,
                       2, 	  	 4, 	  	 "alpha", 	  	 "E", 	  	 112,
                       3, 	  	 4, 	  	 "gamma", 	  	 "A", 	  	 73,
                       4, 	  	 4, 	  	 "epsilon", 	 "B", 	  	 116,
                       5, 	  	 4, 	  	 "beta", 	  	 "C", 	  	 85,
                       1, 	  	 5, 	  	 "epsilon", 	 "E", 	  	 132,
                       2, 	  	 5, 	  	 "beta", 	  	 "A" ,	  	 99,
                       3, 	  	 5, 	  	 "delta", 	  	 "B", 	  	 129,
                       4, 	  	 5, 	  	 "alpha", 	  	 "C", 	  	 100,
                       5,	  	 5, 	  	 "gamma", 	  	 "D", 	  	 100) %>%
  mutate(across(where(is.character),as.factor))


