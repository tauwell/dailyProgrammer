#July 29, 2015
###URL: https://www.reddit.com/r/dailyprogrammer/comments/3f0hzk/20150729_challenge_225_intermediate_estimating_pi/

###Challenge:
# In modern times, if we wish to calculate the value of pi to some precision, there are plenty of mathematical formulas that you can use to get the value. Leibniz formula for pi and the solution to the Basel problem are two of the most famous ones, though both converge very slowly. Modern day computers that attempt to set the world record for digits of pi generally use some variation on Ramanujan's formula, which converges very rapidly.
# 
# However, back in the good old days, we didn't know of these formulas. They all depend on analysis and infinite sums which mathematicians had no skill at manipulating. Back then, the way you estimated pi was less accurate but more straight-forward: you drew a circle, measured it, and calculated pi from that.
# 
# Today, we're going to honor those mathematicians of old. You will be given an image of a black circle on white background, and using the pixel data in the image, you are to come up with an estimate for pi.
# 
# For those of you who have forgotten your formulas for circles, the formula for the area of a circle is as follows:
# 
# A = pi * r^2
# 
# In other words, to calculate the area of a circle, multiply pi by the square of the radius.


# Inputs
# 
# As input, you will be given an image that contains a black circle on white background (those are the only two colors in the image, there's no anti-aliasing or anything). The image provided will be in PNG format, but if you find it difficult to import and analyze PNG images in your language, you're welcome to use a tool like ImageMagick to convert it to a format you prefer (the Netpbm family of formats are famously easy for a computers to parse).
#                                                                                        
#                                                                                        Note that for challenge input 1, the height and width of the image itself is equal to the diameter of the circle, but that is not true for challenge input #2. It is highly encouraged (but not required) that you to try and write a program that works for both challenge inputs. 
# 


library(png)
b <- readPNG("/Users/Cannon/Documents/Programming Stuff/Daily Challenges/July 29 Images/5GScbUe.png")
heatmap(data.matrix(b[,,1]), Rowv = NA, Colv = NA, symm = TRUE)

a <- readPNG("/Users/Cannon/Pictures/Big Truck Bob.png")
heatmap(data.matrix(a[,,1]), Rowv = NA, Colv = NA, symm = TRUE)

