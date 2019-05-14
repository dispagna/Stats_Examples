library(ggplot2)

# Calculate P[ |X - Y| < m ] where X, Y are discrete uniform RVs

N = 100
M = 200
m = 33

# X - Discrete Uniform
x = seq(10, N, 1)
px = rep(1/(max(x) - min(x) + 1), length(x))
X = cbind(x, px)

# Y - Discrete Uniform
y = seq(0, M, 1)
py = rep(1/(max(y) - min(y) + 1), length(y))
Y = cbind(y, py)

# Z = X - Y
z = seq((min(x) - max(y)), (max(x) - min(y)), 1)
pz = rep(0, length(z))
Z = cbind(z, pz)

# pdf of Z is the convolution of X and Y
for(i in 1:length(z)) # z index
{
	sum = 0
	for(j in 1:length(x)) # x index
	{
		temp = X[j,1] - Z[i,1]
		#cat("temp", temp, "\n")
		if ((min(y)<=temp) && (temp<=max(y)))
		{		
			#cat("i, j = ", i, j, "\n")
			sum = sum + X[j, 2] * Y[(y==temp),2]
			#cat("sum = ", sum, "\n")
		}
	}
	Z[i,2] = sum
}

cat("Fz = ", sum(Z[,2]), "(should be 1)\n")
sum(Z[(z > -m) & (z < m),2])
sum(Z[(z >= -m) & (z <= m), 2])
plot(Z, type="s")
abline(v = (max(x)-max(y)), col="red")
abline(v = (min(x)-min(y)), col="blue")

# Using convolve
Ydf = data.frame(y = seq(0,M,1),
                 py = py)
Xdf = data.frame(x = seq(10, N, 1),
                 px = px)

Zdf = data.frame(z = seq(-190, 100, 1),
                 pz = convolve(Ydf$py, Xdf$px, type = "o"))

ggplot() +
  geom_point(data = Ydf, 
             mapping = aes(x = y, y = py, color = "Y")) +
  geom_point(data = Xdf, 
             mapping = aes(x = x, y = px, color = "X")) +
  geom_point(data = Zdf, 
             mapping = aes(x = z, y = pz, color = "convolve"),
             position = "jitter") +
  geom_point(data = as.data.frame(Z), 
             mapping = aes(x = z, y = pz, color = "manual"), 
             alpha = 0.2,
             position = "jitter")

# Now compare to bootstrap
N = 5000
Xs = runif(N, 10, 100)
Ys = runif(N, 1, 200)
Zs = Xs - Ys

sum((Zs > -m) & (Zs < m)) / length(Zs)

ggplot() +
  geom_histogram(data = as.data.frame(Zs),
                 mapping = aes(x = Zs, y = ..density..),
                 binwidth = 10,
                 fill = "grey",
                 color = "black",
                 alpha = 0.2) +
  geom_line(data = as.data.frame(Z),
            mapping = aes(z, pz),
            color = "blue")
