# Another Tokenization Graph

# Table 1 in (Gron and Bertels 2018).

# Reference Paper:
# Leonie Gron and Ann Bertels. 
# Clinical sublanguages: Vocabulary structure and its impact on term weighting. 
# Terminology: International Journal of Theoretical and Applied Issues 
# in Specialized Communication, 24(1):41-65, 2018

# ----------------------------------------------------------------------

# Table 1: "Percentage of tokenization errors using the NLTK tokenizer alone, 
# and in combination with the custom script."

# Standard NLTK tokenizer vs Custom script
# Create a dodged plot and compare the percentages of tokenization errors. 
# Also put the numbers on top of each bar, 
# and remember to include ``(left)'' and ``(right)'' in the legend.
# => I can start making this (lovely) data visualization! 
# No need to request for permission.

# ----------------------------------------------------------------------

library(ggplot2)

section = c("Complaints","Anamnesis","History",
            "Examination","Conclusion","Comments")
# "% errors using NLTK tokenizer alone"
error_before = c(1.36, 2.81, 2.93, 2.59, 2.07, 5.54)
# "% errors using NLTK tokenizer with post-processing"
error_after = c(0.34, 1.1, 0.2, 0.22, 0.41, 2.44)

df = data.frame(section, error_before, error_after)
# ggplot(data=df, aes(x=section, y=error_before)) +
#   geom_bar(stat="identity")

new.df = data.frame(
  section = factor(c(section, section),
          levels=section),
  category = factor(c(rep("error_before",length(error_before)),
          rep("error_after",length(error_after))),
          levels=c("error_before","error_after")),
  value = c(error_before, error_after)
)

# Source code:
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

# ----------------------------------------------------------------------

# % of Tokenization Errors in Electronic Health Records

percent_before = c("1.36%", "2.81%", "2.93%", "2.59%", "2.07%", "5.54%")
percent_after = c("0.34%", "1.10%", "0.20%", "0.22%", "0.41%", "2.44%")

gron2018_table1 = ggplot(data=new.df, aes(x=section, y=value/100, fill=category)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  ggtitle("% of Tokenization Errors in Electronic Health Records") +
  scale_y_continuous(limits = c(0,0.065),
                     labels = function(y) paste0(y*100, "%")) +
  scale_fill_manual(labels=c("NLTK tokenizer only (left)",
                             "NLTK with post-processing (right)"),
                    values=c("red","lightblue")) +
  geom_text(aes(x=section, y=value/100, label=c(percent_before, percent_after)),
            vjust=-0.5, hjust=c(rep(1.35,each=6),rep(-0.35,each=6)),
            color="black", size=5)+
  xlab("Section") + ylab("Errors") + 
  theme_bw(base_size=20) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position="bottom")

gron2018_table1

ggsave("gron2018_table1.png",gron2018_table1,width=13,height=6) 
