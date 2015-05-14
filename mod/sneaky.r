# the sneaky module

# NO BIG FEET!

#custom sd calculator based upon "Eq. S1" and "Eq. S2"
#in Kilmek et al. 
custom_sd <- function(x,center=mean(x)){
        sqrt(mean((x-center)^2))  
}


