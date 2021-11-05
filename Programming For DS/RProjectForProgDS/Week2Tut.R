### 1
# 1 to 10 with increment of 1
seq(from=1, to=10, by=1)
# -10 to 10, 1
seq(from=-10, to=10, by=1)
# -10 to 10, 2
seq(from=-10, to=10, by=2)
# -10 to 10, 3
seq(from=-10, to=10, by=3)
# 0 to 1, 0.1
seq(from=0, to=1, by=0.1)
# 1 to -1, - 0.2
seq(from=1, to=-1, by=-0.2)
# 1 to 256, where each value is double the previous.
2^seq(0,8)
# 1 to 10 by 1 and 12-30 by 2
c(1:10, seq(12,30,2))

### 2 Plotting mathematical functions

x <- -50:50
plot(x, y, type="l")
# y = 3x + 9
y <- 3 * x + 9
# y = 2 x^2 +4x +1
y <- 2 + (x^2) + 4 * x + 1
# 
y <- -x^3 + 4 * (x^2) + 10 * x -7
#
y <- log(x)
#
y <- sin(x)
#
y <- cos(x/10)
#
y <- sin(x/2)/x
#
y <- 1/(1+exp(-x/5))

### 3 

head(morley)
speed <- morley$Speed
speed <- speed + 299000
hist(speed)
mean(speed)
sd(speed)

msol <- mean(speed)
sol <- 299792.458
msol - sol

### 4
measure <- scan(n=1)
print("The difference is")
msol - measure

# determine if it's an outlier or not?
speed_summary <- summary(speed)
lb <- speed_summary[2]
lb <- unname(lb)
ub <- speed_summary[5]
ub <- unname(ub)

### 5

### 6 Cricket Score card
print("Please write numbers of overs completed")
overs_completed <- scan(n=1)
print("Please supply the runs")
runs <- scan(n=1)
print("please supply the target runs")
target <- scan(n=1)

run_rate <- runs/overs_completed
required_run_rate <- (target-runs) / (20 - overs_completed)

cat("    ####################################
    ## Runs:\t\t\t\t", format(runs, width=4), "##
    ## Overs Completed: \t\t", format(overs_completed, width=4), "##
    ## Run Rate:\t\t\t", format(round(run_rate,2), width=4), "##
    ## Required Run Rate: \t\t", format(round(required_run_rate,2), width = 4), "##
    ####################################")



pbinom(c("H", "T"), 100, 0.5)
