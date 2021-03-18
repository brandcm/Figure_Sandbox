# load libraries
library(ggplot2)
library(gganimate)
library(gifski)

# ============ Hard Sweep ============ 

# build a sigmoid function
sigmoid = function(x) {
  1 / (1 + exp(-x - 2))
}

# create vector for x and y and make dataframe to make animation easier
x <- seq(-20, 8, 0.01)
y <- sapply(x, sigmoid)
df <- data.frame(x, y)

# plot and animate
plot <- ggplot(data=df, aes(x=x, y=y)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) + geom_point(size=5) +  theme_classic() + xlab("\n\nTime") + ylab("Allele Frequency\n\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + expand_limits(y=c(0,1))
plot + transition_reveal(x)
anim_save("hard_sweep.gif")

# get still of figure
png("hard_sweep.png", width = 8, height = 8, units = "in", res = 300)
ggplot(data=df, aes(x=x, y=y)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) +  theme_classic() + xlab("\nTime") + ylab("Allele Frequency\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + expand_limits(y=c(0,1))
dev.off()

# ============ de novo Soft Sweep ============ 

# build two sigmoid functions 
sigmoid2 = function(x) {
  0.6 / (1 + exp(-x - 2))
}

sigmoid3 = function(x) {
  0.4 / (1 + exp(-x + 3))
}

# create vector for x and y and make dataframe to make animation easier
x2 <- seq(-20, 8, 0.01)
y2 <- sapply(x2, sigmoid2)
df2 <- data.frame(x2, y2)
df2$allele <- "allele1"
colnames(df2) <- c("x","y","allele")

x3 <- seq(-20, 8, 0.01)
y3 <- sapply(x3, sigmoid3)
df3 <- data.frame(x3, y3)
df3$allele <- "allele2"
colnames(df3) <- c("x","y","allele")

df4 <- rbind(df2,df3)

# plot and animate
plot2 <- ggplot(data=df4, aes(x=x, y=y, color=allele)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) + geom_point(size=5) +  theme_classic() + xlab("\n\nTime") + ylab("Allele Frequency\n\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + scale_color_manual(values=c("deepskyblue4", "mediumpurple4")) + theme(legend.position = "none") + expand_limits(y=c(0,1))
plot2 + transition_reveal(x)
anim_save("soft_sweep_de_novo.gif")

# get figure still

png("soft_sweep_de_novo.png", width = 8, height = 8, units = "in", res = 300)
ggplot(data=df4, aes(x=x, y=y, color=allele)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) +  theme_classic() + xlab("\nTime") + ylab("Allele Frequency\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + scale_color_manual(values=c("deepskyblue4", "mediumpurple4")) + theme(legend.position = "none") + expand_limits(y=c(0,1))
dev.off()

# ============ Soft Sweep from Standing Genetic Variation ============ 

sigmoid5 = function(x) {
  (0.3 / (1 + exp(-x - 2))) + 0.1
}

x5 <- seq(-20, 8, 0.01)
y5 <- sapply(x5, sigmoid5)
df5 <- data.frame(x5, y5)
df5$allele <- "allele1"
colnames(df5) <- c("x","y","allele")

sigmoid6 = function(x) {
  (0.45 / (1 + exp(-x + 2))) + 0.15
}

x6 <- seq(-20, 8, 0.01)
y6 <- sapply(x6, sigmoid6)
df6 <- data.frame(x6, y6)
df6$allele <- "allele2"
colnames(df6) <- c("x","y","allele")

df7 <- rbind(df5,df6)

plot3 <- ggplot(data=df7, aes(x=x, y=y, color=allele)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) + geom_point(size=5) +  theme_classic() + xlab("\n\nTime") + ylab("Allele Frequency\n\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + scale_color_manual(values=c("deepskyblue4", "mediumpurple4")) + theme(legend.position = "none") + expand_limits(y=c(0,1))
plot3 + transition_reveal(x)
anim_save("soft_sweep_sgv.gif")

png("soft_sweep_sgv.png", width = 8, height = 8, units = "in", res = 300)
ggplot(data=df7, aes(x=x, y=y, color=allele)) + geom_vline(xintercept=c(-9), linetype="dashed") + geom_line(size=2.5) + theme_classic() + xlab("\nTime") + ylab("Allele Frequency\n") + theme(axis.text.x = element_blank()) + theme(axis.text=element_text(size=22),axis.title=element_text(size=30,face="bold")) + scale_color_manual(values=c("deepskyblue4", "mediumpurple4")) + theme(legend.position = "none") + expand_limits(y=c(0,1))
dev.off()