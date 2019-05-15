library(ggplot2)

#
# Part 1: Z = X + Y (Discrete Uniform Distributions)
# Following parameters are equivalent to Part 2 with U[0,1] parameters
# xStart = 0
# xEnd = 1
# yStart = 0
# yEnd = 1
# delta = 0.01
#
xStart = 0
xEnd = 100
yStart = 0
yEnd = 200
delta = 1

Xdf = data.frame(x = seq(xStart, xEnd, delta),
                 px = dunif(seq(xStart, xEnd, delta), xStart, xEnd))

Ydf = data.frame(y = seq(yStart, yEnd, delta),
                 py = dunif(seq(yStart, yEnd, delta), yStart, yEnd))

# Get theoretical distribution of Z = X + Y using convolve
Zdf = data.frame(z = seq((xStart + yStart), (xEnd + yEnd), delta),
                 pz = delta * convolve(Ydf$py, Xdf$px, type = "o"))

ggplot() +
  geom_point(data = Ydf, 
             mapping = aes(x = y, y = py, color = "Y")) +
  geom_point(data = Xdf, 
             mapping = aes(x = x, y = px, color = "X")) +
  geom_point(data = Zdf, 
             mapping = aes(x = z, y = pz, color = "Z = X + Y"))

# Now compare to sampling Z directly
N = 5000
Xs = runif(N, xStart, xEnd)
Ys = runif(N, yStart, yEnd)
Zs = Xs + Ys

ggplot() +
  geom_histogram(data = as.data.frame(Zs),
                 mapping = aes(x = Zs, y = ..density..),
                 binwidth = delta * 10,
                 fill = "grey",
                 color = "black",
                 alpha = 0.2) +
  geom_line(data = Zdf,
            mapping = aes(z, pz),
            color = "blue")

#
# Part 2: Z = X + Y (Beta Distributions, set alpha = beta = 1 for U[0,1])
#
xalpha = 2
xbeta = 2
yalpha = 2
ybeta = 5
bdelta = 0.01
Xdf = data.frame(x = seq(0, 1, bdelta),
                 px = dbeta(seq(0, 1, bdelta), xalpha, xbeta))

Ydf = data.frame(y = seq(0, 1, bdelta),
                 py = dbeta(seq(0, 1, bdelta), yalpha, ybeta))

# Get theoretical distribution of Z = X + Y using convolve
Zdf = data.frame(z = seq((min(Xdf$x) + min(Ydf$y)), (max(Xdf$x) + max(Ydf$y)), bdelta),
                 pz = bdelta * convolve(Ydf$py, Xdf$px, type = "o"))

ggplot() +
  geom_point(data = Ydf, 
             mapping = aes(x = y, y = py, color = "Y")) +
  geom_point(data = Xdf, 
             mapping = aes(x = x, y = px, color = "X")) +
  geom_point(data = Zdf, 
             mapping = aes(x = z, y = pz, color = "Z = X + Y"))


# Now compare to sampling Z directly
N = 5000
Xs = rbeta(N, xalpha, xbeta)
Ys = rbeta(N, yalpha, ybeta)
Zs = Xs + Ys

ggplot() +
  geom_histogram(data = as.data.frame(Zs),
                 mapping = aes(x = Zs, y = ..density..),
                 binwidth = 0.05,
                 fill = "grey",
                 color = "black",
                 alpha = 0.2) +
  geom_line(data = Zdf,
            mapping = aes(z, pz),
            color = "blue")

#
# Part 3: Z = X + Y - X*Y (Sum of Probabilities)
# X, Y, Z now represent distributions of probabilities
#

# Can we just scale support of Zdf to [0,1]?  Doesn't look like we can...
Zdf_scaled = data.frame(z = Zdf$z/2,
                        pz = Zdf$pz)

Zs2 = Xs + Ys - (Xs * Ys)

ggplot() +
  geom_histogram(data = as.data.frame(Zs2),
                 mapping = aes(x = Zs2, y = ..density..),
                 binwidth = 0.05,
                 fill = "grey",
                 color = "black",
                 alpha = 0.2) +
  geom_line(data = Zdf_scaled,
            mapping = aes(z, pz),
            color = "blue")


#
# Part 4: TBD - something other than ordinary convolution to calculate
#               theoretical distribution of Z = X + Y - X*Y
#
