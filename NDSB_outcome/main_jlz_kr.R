# PURPOSE: plot precision heat map for top 10 Kaggle competition solutions

# CREATED: 29-March-2015
# AUTHOR: Jesscia Luo
# MODIFIED LAST: 27-Jan-2016, Kelly Robinson

library(plyr)
library(reshape2)
library(ggplot2)
library(caret)
library(RColorBrewer)
library(e1071)
library(stringr)
library(scales)
library(colorspace)

# FUNCTIONS -----
  get_predicted_class <- function(p) {
    return(names(which.max(p)))
  }
  
  # Spectral colour map from ColorBrewer
  spectral <- function(n=6) {
    library("RColorBrewer")
    rev(brewer.pal(name="Spectral", n=n))
  }
  
  scale_fill_spectral <- function(...) {
    scale_fill_gradientn(colours=spectral(...))
  }
  scale_colour_spectral <- function(...) {
    scale_colour_gradientn(colours=spectral(...))
  }


## { Calculate accuracy and results for top 10 ------------------------------------
submissions <- list.files("Solution/sub", full=T, recursive=T, pattern = ".csv" )

# read in solutions
sol <- read.csv("Solution/solution.csv", stringsAsFactors = F)

# Image name order may not match
sol <- arrange(sol, image)

# Pull out the test set
test_idx = which(sol$Usage %in% c("Public", "Private"))
sol <- sol[test_idx,]

levels <- names(sol)[2:122] # Skip the image names at the front, and the usage label at the end

actual <- factor(apply(sol[,2:122], 1, get_predicted_class), levels = levels)


subEval <- adply(submissions, 1, function(x){
	#x <- submissions[1]
  sub <- read.csv(x)
	name <- str_split_fixed((dirname(x))," ",2)[,2] #just using 'x' returned the whole path name
	RANK <- str_sub(x, 7, 8) #changed from "str_sbu(x, 5, 6) because that returned only "00" for all files
	sub <- arrange(sub, image)
	sub <- sub[test_idx,]
	
	preds  <- factor(apply(sub[,2:122], 1, get_predicted_class), levels = levels)
	
	# Overall accuracy
	accuracy = sum(preds == actual) / length(actual)

	# pull out the fish accuracy
	fish_grps <- which(str_detect(actual, "fish"))
	fish_accuracy <- sum(preds[fish_grps]==actual[fish_grps]) / length(fish_grps)
	fish_grp_accuracy <- length(which(str_detect(preds[fish_grps], "fish"))) / length(fish_grps)
	
	# Make confusion matrix and pull out statistics
	X <- confusionMatrix(preds, actual)
	precision <- X$byClass[,"Pos Pred Value"]
	recall <- X$byClass[,"Sensitivity"]
	results <- data.frame(precision, recall)
	write.csv(results, paste0("Solution/res/", RANK, "_", name, ".csv"), row.names=T)
	
# 	# Plot a confusion matrix
# 	X <- as.table(X)
# 	jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
# 	jBuPuPalette <- jBuPuFun(256)
# 	pdf(paste0("Solution/plots/heatmap_", RANK, "_", name,".pdf"), height=11, width=11)
# 	heatmap(as.matrix(X), Rowv = NA, Colv = NA, col = jBuPuPalette, scale = "col", margins = c(14,14))
# 	dev.off()
	
	return(data.frame(accuracy, fish_accuracy, fish_grp_accuracy))
	
  }, .progress = "text")

group_names <- str_split_fixed(submissions,"/", 4)[,3]
subEval <- data.frame(group_names, subEval[,-1])
write.csv(subEval, "Solution/res/Top10_accuracies.csv", row.names=F)

#}

##{ Calculate average precisions ---------------------------------------
f <- list.files("Solution/res/results", pattern="results*", full=T)

# read all in
d <- adply(f, 1, read.csv)

names(d) <- c("team", "class", "precision", "recall")
d$team <- as.character(d$team)
d$class <- as.character(d$class)
d <- d[,c("team", "class", "precision")]

#Remove "Class: "from class names in "d"
d$class <- str_split_fixed((d$class),": ", 2)[,2]

# read in grouped class names
d2 <-read.csv("Solution/kaggle_classes_renaming.csv",header=F, skip=1, stringsAsFactors = F)
names(d2) <- c("class","aggclass","group")
d2$class <- as.character(d2$class)
d2$aggclass <- as.character(d2$group)
d2 <- d2[, c("class", "group")]

# join df with precision of Kaggle classes to df with aggregated class names
d3 <- merge(x = d, y = d2, by.x ="class", by.y = "class", all.x = TRUE)

# cast into wide df for easy rowMeans calculation
dc <- dcast(d3, group~team, fun.aggregate = mean, value.var="precision", na.rm = T) #added aggregation function 'mean' to dcast b/c was defaulting to length
#dc <- dc[,c("aggclass","1","2","3","4","5","6","7","8","9","10")] #top 10
dc <- dc[,c("group","1","2","3","4","5")] #top 5

#write.csv(dc, "res/Top10_meanPrecision.csv", row.names=T)

# plotting
dm <- melt(dc, id.vars = "group")
dm <- ddply(dm, .(variable), transform, rescale = rescale(value))

#plot parameters (Figure 4 in 'Tale of Two Crowds")
#make aggclass a factor with names as levels so you can plot them alphabetically
dm <- within(dm, group <- ordered(group, levels = rev(sort(unique(group)))))

p <- ggplot(dm, aes(x = variable, y = group, fill = value, order = -value)) + geom_tile(show.legend = T, aes()) +
     scale_fill_gradient(high = diverge_hcl(n = 10), na.value = NA, name = "precision", breaks=seq(0, 1, by=0.1)) +
     theme_bw(base_size = 30) + labs(x = "Team rank", y = "General plankton class") +
     scale_x_discrete(expand = c(0, 0), limits = c(1,2,3,4,5)) +
     scale_y_discrete(expand = c(0, 0)) +
     theme(axis.text = element_text(size = 30, hjust = 0, colour = "black")) +
     theme(axis.title.y = element_text(size = 30, face = "bold"), axis.title.x = element_text(size = 30, face = "bold")) +
     theme(legend.text= element_text(size = 30), legend.title = element_text(size = 30), legend.key.size = unit(3, "cm"), legend.key.width = unit(1, "cm"))
     theme(axis.line.y = element_line(colour = "black", size = 1), axis.line.x = element_line(colour = "black", size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks = element_line(size = 1))

plot(p)     
     
png(file = "precision_top5_201608.png", width = 20, height = 30, units = "in", res = 300)
plot(p)
dev.off()

#mean Precision for top 10 teams
meanPrecision <- rowMeans(dc[,2:11])
meanPrecision <- as.data.frame(meanPrecision)

row.names(meanPrecision) <- dc$aggclass


# calculate a difference in Precision values
dP <- ddply(d3, ~team, function(x){
	dPrecision <- x$precision - meanPrecision$meanPrecision
	return(data.frame(class=x$aggclass, dPrecision))
})

# recast for eventual plotting
dPc <- dcast(dP, class~team, mean, na.rm = T, value.var="dPrecision")

#row.names(dPc) <- str_sub(dPc$aggclass, 8, -1)
row.names(dPc) <- dPc[,1]

# get rid of explicit class column
dPc <- dPc[,-1]

names(dPc) <- c("DeepSea", "HappyLanternFestival", "PoissonProcess", "Junonia",
"DeepseaChallenger", "AuroraXie", "MaximMilakov", "IlyaKostrikov", "oldufo", "nagadomi")

# plotting
jRYGFun <- colorRampPalette(brewer.pal(n = 9, "RdYlGn"))
jRYGPalette <- jRYGFun(256)
pdf("plots/heatmap_diffPrecision.pdf", height=11, width=11)
heatmap(as.matrix(dPc), Rowv=NA, Colv = NA, margins=c(8, 13), col=jRYGPalette, scale = "row", cexCol = 0.9, na.rm=T)
dev.off()

#}

