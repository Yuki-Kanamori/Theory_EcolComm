set.seed(1)

j = 50
init.1= j/2
com = vector(length = j)
com[1:init.1] = 1
com[(init.1+1):j] = 2
num.year = 50
year = 2

freq.1.vec = vector(length = num.year)
freq.1.vec[1] = init.1/j #種1の初期頻度

for(i in 1:(j*num.year - 1)){
  # i = 1
  freq.1 = sum(com == 1)/j
  pr.1 = freq.1
  com[ceiling(j*runif(1))] = sample(c(1, 2), 1, prob = c(pr.1, 1 - pr.1))
  com
  
  if(i %% j == 0){# 余り 局所群集50個分（つまり1年分）が終わったら，その年の頻度を計算して次の年に行くようにしている
    freq.1.vec[year] = sum(com == 1)/j
    year = year + 1
  }
}

plot(1:num.year, freq.1.vec, type = "1", xlab = "Time", ylab = "Frequency of species 1", ylim = c(0, 1))

require(tidyverse)
freq = data.frame(year = rep(1:j), freq = freq.1.vec)
g = ggplot(data = freq, aes(x = year, y = freq))
l = geom_line()
g+l+theme_bw()
