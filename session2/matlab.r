require(R.matlab)

path <- system.file("mat-files", package="R.matlab")
pathname <- file.path(path, "ABC.mat")
mdata <- readMat(pathname)
print(data)


pathname <- file.path(path, "NestedMiMATRIX,problem1,v5,compressed.mat")
mdata <- readMat(pathname)


pathname <- file.path(path, "TextWithNewlines.mat")
mdata <- readMat(pathname)


## http://sccn.ucsd.edu/~arno/fam2data/publicly_available_EEG_data.html
## https://sites.google.com/site/projectbci/
mdata <- readMat("matlab/Subject1_2D.mat")

lapply(mdata,class)
lapply(mdata,dim)
