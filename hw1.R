res = read.csv(file="04cars.csv", header=T)
retail_price = as.array(res[,10])
dealer_price = as.array(res[,11])
avg_price = (retail_price + dealer_price)/2
hp = as.array(res[,14])
city_mpg = as.array(res[,15])
hwy_mpg = as.array(res[,16])
#Plot the graph
x = as.numeric(avg_price)
y = as.numeric(hp)
plot(x, y, xlab = "Price($)", ylab = "HP(hp)", col = "blue", pch = 16, xlim = c(0,200000),ylim = c(70,500))
axis(side = 1, at = seq(0,200000, by = 10000),labels = FALSE, tcl = -0.2)
axis(side = 2, at = seq(70,500, by = 10),labels = FALSE, tcl = -0.2)

#Add the regression line on the graph
model = lm(y ~ log(x))
y = predict(model, newdata = list(x))
matlines(x, y, lwd = 2, col = "red")

#Add the legend on the graph
legend("bottomright",legend = c("raw data", "log regression line"), col = c("blue", "red"), cex = 0.8, lty = c(NA, 1), pch = c(16, NA))
#Plot the graph
x = as.numeric(avg_price)
y = as.numeric(city_mpg)
plot(x, y, xlab = "Price($)", ylab = "City MPG(Miles per Gallon)", col = "blue", pch = 16, xlim = c(0,200000) , ylim = c(10,70))
axis(side = 1, at = seq(0,200000, by = 10000),labels = FALSE, tcl = -0.2)
axis(side = 2, at = seq(10,70, by = 2),labels = FALSE, tcl = -0.2)

#Add the regression line on the graph
model = lm(y ~ log(x))
y = predict(model, newdata = list(x))
matlines(x, y, lwd = 2, col = "red")

#Add the legend on the graph
legend("topright",legend = c("raw data", "log regression line"), col = c("blue", "red"), cex = 0.8, lty = c(NA, 1), pch = c(16, NA))
#Plot the graph
x = as.numeric(avg_price)
y = as.numeric(hwy_mpg)
plot(x, y, xlab = "Price($)", ylab = "Highway MPG(Miles per Gallon)", col = "blue", pch = 16, xlim = c(0,200000) , ylim = c(10,70))
axis(side = 1, at = seq(0,200000, by = 10000), labels = FALSE, tcl = -0.2)
axis(side = 2, at = seq(10,70, by = 2), labels = FALSE, tcl = -0.2)

#Add the regression line on the graph
model = lm(y ~ log(x))
y = predict(model, newdata = list(x))
matlines(x, y, lwd = 2, col = "red")

#Add the legend on the graph
legend("topright",legend = c("raw data", "log regression line"), col = c("blue", "red"), cex = 0.8, lty = c(NA, 1), pch = c(16, NA))