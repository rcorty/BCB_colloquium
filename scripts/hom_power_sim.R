library(CortyKit)
library(dglm)
library(MASS)

dglm.ml <- function(...) { -dglm(..., method = 'ml')$m2loglik }
mean.test <- function(resp) {
    LLj <- dglm.ml(formula = resp ~  q, dformula = ~ q)
    LLv <- dglm.ml(formula = resp ~ 1, dformula = ~ q)
    return(LLj - LLv)
}


set.seed(27599)

n.tests <- 1e3
n.obs <- 1e2
betas <- (0:10)/20

res <- list()
pb <- txtProgressBar(min = 1, max = n.tests)
for (test.idx in 1:n.tests) {
    
    setTxtProgressBar(pb = pb, value = test.idx)
    q.num <- sample(x = c(0, 1, 2), size = n.obs, replace = TRUE) 
    q <- factor(x = q.num)
    
    dglm.p <- lm.p <- rep(NA, length(betas))
    for (beta.idx in 1:length(betas)) {
        
        y <- rnorm(n = n.obs, mean = q.num*betas[beta.idx])
        
        dglm.p[beta.idx] <- pchisq(q = mean.test(y), df = 2, lower.tail = FALSE)
        lm.p[beta.idx] <- pchisq(q = 2*logLik(lm(y ~ q)) - 2*logLik(lm(y ~ 1)), df = 2, lower.tail = FALSE)
    }
    
    res[[test.idx]] <- data_frame(test.idx = test.idx,
                                  beta = rep(betas, 2),
                                  test = rep(c('lm', 'dglm'), each = length(betas)),
                                  p = c(lm.p, dglm.p))
}


res2 <- bind_rows(res) 

res2 %>% 
    group_by(test, beta) %>% 
    summarise(pos.rate = mean(p < 0.05)) %>% 
    mutate(se = pos.rate*(1-pos.rate)/sqrt(n.tests)) %>% 
    ggplot(mapping = aes(x = beta, y = pos.rate, color = test)) + 
    geom_ribbon(mapping = aes(ymin = pos.rate - se, ymax = pos.rate + se, fill = test), color = NA, alpha = 0.3) +
    geom_point() +
    xlab('effect size') +
    ylab('positive rate') 
ggsave(filename = 'slides/supporting_files/figs/hom_power.pdf', width = 5, height = 4)

