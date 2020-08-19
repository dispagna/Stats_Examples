bindSamples <- function(samplesX, samplesY, samplesZ)
{
  df <- rbind(data.frame(Brand = "Cheerios",
                         values = samplesX),
              data.frame(Brand = "Froot Loops",
                         values = samplesY),
              data.frame(Brand = "Raisin Bran",
                         values = samplesZ)) 
}
