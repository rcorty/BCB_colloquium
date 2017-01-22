library(ggplot2)
library(wesanderson)
library(RColorBrewer)

# two-level factors
ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.1), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '2'), geom = 'density', fun = dnorm, args = list(mean = 0.1), alpha = 0.5, color = NA) +
    scale_fill_brewer(type = 'qual', palette = 'Set1') +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/mean_effect_two_groups.pdf', height = 4, width = 7)


ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = 'SD = 0.9'), geom = 'density', fun = dnorm, args = list(sd = 0.9), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = 'SD = 1.1'), geom = 'density', fun = dnorm, args = list(sd = 1.1), alpha = 0.5, color = NA) +
    scale_fill_brewer(type = 'qual', palette = 'Set1') +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')
    
ggsave(filename = 'slides/supporting_files/figs/var_effect_two_groups.pdf', height = 4, width = 7)


ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = 'SD = 0.9'), geom = 'density', fun = dnorm, args = list(mean = 0.1, sd = 0.9), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = 'SD = 1.1'), geom = 'density', fun = dnorm, args = list(mean = -0.1, sd = 1.1), alpha = 0.5, color = NA) +
    scale_fill_brewer(type = 'qual', palette = 'Set1') +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/meanvar_effect_two_groups.pdf', height = 4, width = 7)



# seven-level factors
ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.3), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '2'), geom = 'density', fun = dnorm, args = list(mean = -0.2), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '3'), geom = 'density', fun = dnorm, args = list(mean = -0.1), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '4'), geom = 'density', fun = dnorm, args = list(mean = 0), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '5'), geom = 'density', fun = dnorm, args = list(mean = 0.1), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '6'), geom = 'density', fun = dnorm, args = list(mean = 0.2), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '7'), geom = 'density', fun = dnorm, args = list(mean = 0.3), alpha = 0.5, color = NA) +
    scale_fill_brewer(type = 'qual', palette = 'Set1') +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/mean_effect_seven_groups.pdf', height = 4, width = 7)


ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 0.7), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '2'), geom = 'density', fun = dnorm, args = list(sd = 0.8), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '3'), geom = 'density', fun = dnorm, args = list(sd = 0.9), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '4'), geom = 'density', fun = dnorm, args = list(sd = 1.0), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '5'), geom = 'density', fun = dnorm, args = list(sd = 1.1), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '6'), geom = 'density', fun = dnorm, args = list(sd = 1.2), alpha = 0.5, color = NA) +
    stat_function(mapping = aes(fill = '7'), geom = 'density', fun = dnorm, args = list(sd = 1.3), alpha = 0.5, color = NA) +
    scale_fill_brewer(type = 'qual', palette = 'Set1') +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/var_effect_seven_groups.pdf', height = 4, width = 7)





### all in gray
# two-level factors
ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.1), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = 0.1), color = NA) +
    theme_minimal() +
    scale_fill_grey(start = 0.5, end = 0.5) +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/gray_mean_effect_two_groups.pdf', height = 4, width = 7)


ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 0.9), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 1.1), color = NA) +
    scale_fill_grey(start = 0.5, end = 0.5) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/gray_var_effect_two_groups.pdf', height = 4, width = 7)



# seven-level factors
ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.3), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.2), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = -0.1), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = 0), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = 0.1), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = 0.2), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(mean = 0.3), color = NA) +
    scale_fill_grey(start = 0.5, end = 0.5) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/gray_mean_effect_seven_groups.pdf', height = 4, width = 7)


ggplot(mapping = aes(x = -3:3, fill = )) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 0.7), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 0.8), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 0.9), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 1.0), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 1.1), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 1.2), color = NA) +
    stat_function(mapping = aes(fill = '1'), geom = 'density', fun = dnorm, args = list(sd = 1.3), color = NA) +
    scale_fill_grey(start = 0.5, end = 0.5) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          legend.position = 'none')

ggsave(filename = 'slides/supporting_files/figs/gray_var_effect_seven_groups.pdf', height = 4, width = 7)
