#!/usr/bin/env Rscript

#USAGE:
#./plot_metagenome_contributions.R <input file from picrust> <output pdf file> <function> <taxa rank>
#
#For example:
#./plot_metagenome_contributions.R metagenome_contributions.tab K00001_family.pdf K00001 Family
#OR 
#./plot_metagenome_contributions.R metagenome_contributions.tab K00001_phylum.pdf K00001 Phylum
#
#Choices for taxa are: "Phylum",'Class','Order','Family','Genus','Species','OTU' 

library(ggplot2)
Args <- commandArgs(TRUE)

input_file<-Args[1]
output_file<-Args[2]
function_id<-Args[3]
taxa_rank<-Args[4]

#read in the file created by PICRUSt's metagenome_contributions.py  
d<-read.delim(input_file)

#Pull out only one gene for plotting
d<-d[d$Gene == function_id,]

#Filter out OTUs that only contribute a small amount of functions (e.g. 1 or less)
#Increase this number if too many taxa labels are showing up in the legend. 
otu_contribution_cut_off<-0 #0 means no filter being applied
d<-d[d$CountContributedByOTU > otu_contribution_cut_off,]

#output image to pdf (this can be change to png, svg, etc)
pdf(output_file)

#create stacked bar chart, with x-axis being samples, collapsing to user defined taxonomic rank, and weighted by actual OTU abundances
qplot(factor(Sample), data=d, geom="bar",fill=factor(d[,taxa_rank]),weight=CountContributedByOTU, xlab="Samples", ylab=paste("Abundance of ",function_id)) +

#legend title 
scale_fill_discrete(taxa_rank) + 

#rotate x axis labels
theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

graphics.off()

