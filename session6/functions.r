getS3method("print","Anova.mlm")

get.p.vals <- function(x){
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, ntests, 4)

    if (!repeated) 
        SSPE.qr <- qr(x$SSPE)
    
    for (term in 1:ntests) {
        eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr, 
            x$SSP[[term]]), symmetric = FALSE)$values)
        tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(eigs, 
            x$df[term], x$error.df), Wilks = stats:::Wilks(eigs, x$df[term], 
            x$error.df), `Hotelling-Lawley` = stats:::HL(eigs, x$df[term], 
            x$error.df), Roy = stats:::Roy(eigs, x$df[term], x$error.df))
    }
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3], 
        tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- x$terms
    colnames(tests) <- c("Df", "test stat", "approx F", "num Df", 
        "den Df", "Pr(>F)")
    return(tests)
}



get.p.vals <- function(x){
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, 1, 4)

    if (!repeated) 
        SSPE.qr <- qr(x$SSPE)
    
    for (term in 1:ntests) {
        eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr, 
            x$SSP[[term]]), symmetric = FALSE)$values)
        tests <- rbind(tests,rbind(stats:::Pillai(eigs, x$df[term], x$error.df),
                                   stats:::Wilks(eigs, x$df[term], x$error.df),
                                   stats:::HL(eigs, x$df[term], x$error.df),
                                   stats:::Roy(eigs, x$df[term], x$error.df)))
    }

    tests <- tests[-1,]
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(rep(x$df,each=4), tests, pf(tests[ok, 2], tests[ok, 3], 
        tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- paste(rep(x$terms,each = 4),c("Pillai","Wilks","HL","Roy"))
    colnames(tests) <- c("Df", "test stat", "approx F", "num Df", 
        "den Df", "Pr(>F)")
    return(tests)
}

#############################################################################
sentences <- read.csv("sentences.csv")

cols <- names(sentences)[grep("PDWnocc_|PDWhkl_",names(sentences))]
xx <- reshape(sentences[,cols],varying = cols, sep="_",direction = "long")

