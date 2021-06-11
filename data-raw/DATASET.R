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

epitaxial  = cbind.data.frame(design,ybar, s2, lns2)


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
