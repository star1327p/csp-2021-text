# Tokenization Graph

# Table 2 in (Cohen et al. 2002).

# Reference Paper:
# K Bretonnel Cohen, George K Acquaah-Mensah, Andrew E Dolbey, and Lawrence Hunter. 
# Contrast and variability in gene names. 
# In Proceedings of the ACL-02 Workshop on Natural Language Processing in the
# Biomedical Domain, volume 3, pages 14-20. 
# Association for Computational Linguistics (ACL), 2002.

# ----------------------------------------------------------------------

# Table 2: "Names found by strict pattern match, heuristics, and plurals."

# "All heuristics were effective in locating more names 
#  than strict pattern matches alone."

# e.g. Optional hyphenation heuristic allows the official gene name
# "alpha-2-macroglobulin" to match "alpha 2-macroglobulin".

# ----------------------------------------------------------------------

# Create a barplot and list these categories in descending order, 
# with each number on top of the bar.
# => I can start making this (lovely) data visualization! 
# No need to request for permission.

# Original Table:

# Names located by strict pattern matching: 1846
# Additional names located by vowel sequence heuristic matches: 586
# Additional names located by optional hyphen heuristic matches: 37
# Additional names located by case insensitive heuristic matches: 864
# Additional names located by optional parentheses heuristic matches: 432
# Additional names located by plural matches: 87

library(ggplot2)

# dummy = c("gene","gene","gene","gene","gene","gene")
# Make the x-axis name a blank string
dummy = c("","","","","","")
# pattern_simple = c("strict","vowel","hyphen","case","parentheses","plural")
pattern = c("strict pattern matching (1846)", "vowel sequence (586)", 
            "hyphen as optional (37)", "case insensitive (864)",
            "parentheses as optional (432)", "plural matches (87)")
found = c(1846,586,37,864,432,87)
pattern_factor = factor(pattern, ordered=TRUE,
    levels=pattern[order(found, decreasing = TRUE)]) # descending order                        

df = data.frame(dummy,pattern_factor,found)

# Create two versions
# 1. With the legend
# 2. Without the legend

# Actually I prefer the graph with the legend,
# but I also want to create the version without the legend just in case.
# Add the text and number (white color) in the middle of the bar.

# ----------------------------------------------------------------------

# geom_text(): hjust and vjust are between 0 and 1.
# 0 = left-justified, 1 = right-justified.

# In geom_text, aesthetics must be either length 1 or the same as the data (6): label.
labels_text = c(1846,864,586,432,"","")
labels_x = c("","","","","","")
labels_y = c(1000,2250,3000,3500,3501,3502)
# The last two labels are null, so the y position does not matter.

# Graph with legend
cohen2002_table2_legend = ggplot(df, aes(x=dummy, y=found, fill=pattern_factor)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  ggtitle("Official gene name: alpha-2-macroglobulin") +
  xlab("") + ylab("Number of gene names found") +
  geom_text(aes(label = labels_text), col = "white", 
            x = labels_x, y = labels_y, size = 8, hjust=0.5) +
  guides(fill=guide_legend(title="Type of pattern matching")) +
  theme_bw(base_size=24) +
  theme(plot.title = element_text(hjust = 0.5))

cohen2002_table2_legend  
  
ggsave("cohen2002_table2_legend.png",cohen2002_table2_legend,
       width=12,height=4)

# ----------------------------------------------------------------------

labels_simple = c("strict","case insensitive","vowel","( )","","")
# pattern_simple = c("strict","vowel","hyphen","case","parentheses","plural")

# Graph without legend
cohen2002_table2_none = ggplot(df, aes(x=dummy, y=found, fill=pattern_factor)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  ggtitle("Official gene name: alpha-2-macroglobulin") +
  xlab("") + ylab("Number of gene names found by pattern matching") +
  geom_text(aes(label = labels_text), col = "white", 
            x = labels_x, y = labels_y, size = 6, hjust=0.5, vjust=2) +
  geom_text(aes(label = labels_simple), col = "white", 
            x = labels_x, y = labels_y, size = 6, hjust=0.5, vjust=-2) +
  theme_bw(base_size=24) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
cohen2002_table2_none 

ggsave("cohen2002_table2_none.png",cohen2002_table2_none,
       width=11,height=4)  

