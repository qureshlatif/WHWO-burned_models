Maxent_TB <-
  function(brn_1ha, brn_1km, pipo_1km) {
    #Define features and parameters
    l.brn_1ha <- c(0.0, 0.0, 100.0)
    l.brn_1km <- c(-2.3306701540316377, 10.4136110296275, 94.5438545027867)
    l.pipo_1km <- c(0.0, 38.0, 91.0)
    l.brn_1ha2 <- c(1.333223271874084, 0.0, 10000.0)
    l.brn_1km2 <- c(-1.0757281541694295, 108.44329467637952, 8938.540424244102)
    l.brn1haXpipo <- c(2.6469208167976404, 0.0, 9100.0)
    linPN <- 3.3758914329866694
    densNorm <- 871.6706949013783
    entropy <- 8.764537443009441
    
    # Compile higher order features and clamping #
    brn_1ha[which(brn_1ha>l.brn_1ha[3])] <- l.brn_1ha[3]
    brn_1ha[which(brn_1ha<l.brn_1ha[2])] <- l.brn_1ha[2]
    
    brn_1km[which(brn_1km>l.brn_1km[3])] <- l.brn_1km[3]
    brn_1km[which(brn_1km<l.brn_1km[2])] <- l.brn_1km[2]
    
    pipo_1km[which(pipo_1km>l.pipo_1km[3])] <- l.pipo_1km[3]
    pipo_1km[which(pipo_1km<l.pipo_1km[2])] <- l.pipo_1km[2]

    brn_1ha2 <- brn_1ha^2
    brn_1ha2[which(brn_1ha2>l.brn_1ha2[3])] <- l.brn_1ha2[3]
    brn_1ha2[which(brn_1ha2<l.brn_1ha2[2])] <- l.brn_1ha2[2]
    
    brn_1km2 <- brn_1km^2
    brn_1km2[which(brn_1km2>l.brn_1km2[3])] <- l.brn_1km2[3]
    brn_1km2[which(brn_1km2<l.brn_1km2[2])] <- l.brn_1km2[2]
    
    brn1haXpipo <- brn_1ha*pipo_1km
    brn1haXpipo[which(brn1haXpipo>l.brn1haXpipo[3])] <- l.brn1haXpipo[3]
    brn1haXpipo[which(brn1haXpipo<l.brn1haXpipo[2])] <- l.brn1haXpipo[2]
    
    # Apply model #
    exponent <- (l.brn_1ha[1]*((brn_1ha-l.brn_1ha[2])/(l.brn_1ha[3]-l.brn_1ha[2])) +
                   l.brn_1km[1]*((brn_1km-l.brn_1km[2])/(l.brn_1km[3]-l.brn_1km[2])) +
                   l.pipo_1km[1]*((pipo_1km-l.pipo_1km[2])/(l.pipo_1km[3]-l.pipo_1km[2])) +
                   l.brn_1ha2[1]*((brn_1ha2 -l.brn_1ha2[2])/(l.brn_1ha2[3]-l.brn_1ha2[2])) +
                   l.brn_1km2[1]*((brn_1km2-l.brn_1km2[2])/(l.brn_1km2[3]-l.brn_1km2[2])) +
                   l.brn1haXpipo[1]*((brn1haXpipo-l.brn1haXpipo[2])/(l.brn1haXpipo[3]-l.brn1haXpipo[2]))
    ) - linPN
    mx.raw  <- exp(exponent)/densNorm
    hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
    out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
    return(out)
  }

Maxent_CC <-
  function(brn_1ha, brn_1km, slope) {
    #Define features and parameters
    l.slope <- c(0.0, 0.0, 78.7753)
    l.brn_1ha <- c(1.8348128017050744, 0.0, 100.0)
    l.brn_1km <- c(0.0, 17.9524787327662, 96.6265767087122)
    l.brn_1km2 <- c(-1.6066365302472865, 322.2914926504227, 9336.695326444642)
    l.slpXbrn1km <- c(-2.910692604731849, 0.0, 7258.2345937225)
    linPN <- 1.7052420357917026
    densNorm <- 3837.7865054227036
    entropy <- 9.683129219921879
    
    # Compile higher order features and clamping #
    slope[which(slope>l.slope[3])] <- l.slope[3]
    slope[which(slope<l.slope[2])] <- l.slope[2]

    brn_1ha[which(brn_1ha>l.brn_1ha[3])] <- l.brn_1ha[3]
    brn_1ha[which(brn_1ha<l.brn_1ha[2])] <- l.brn_1ha[2]
    
    brn_1km[which(brn_1km>l.brn_1km[3])] <- l.brn_1km[3]
    brn_1km[which(brn_1km<l.brn_1km[2])] <- l.brn_1km[2]

    brn_1km2 <- brn_1km^2
    brn_1km2[which(brn_1km2>l.brn_1km2[3])] <- l.brn_1km2[3]
    brn_1km2[which(brn_1km2<l.brn_1km2[2])] <- l.brn_1km2[2]
    
    slpXbrn1km <- slope*brn_1km
    slpXbrn1km[which(slpXbrn1km>l.slpXbrn1km[3])] <- l.slpXbrn1km[3]
    slpXbrn1km[which(slpXbrn1km<l.slpXbrn1km[2])] <- l.slpXbrn1km[2]
    
    # Apply model #
    exponent <- (l.slope[1]*((slope-l.slope[2])/(l.slope[3]-l.slope[2])) +
                   l.brn_1ha[1]*((brn_1ha-l.brn_1ha[2])/(l.brn_1ha[3]-l.brn_1ha[2])) +
                   l.brn_1km[1]*((brn_1km-l.brn_1km[2])/(l.brn_1km[3]-l.brn_1km[2])) +
                   l.brn_1km2[1]*((brn_1km2-l.brn_1km2[2])/(l.brn_1km2[3]-l.brn_1km2[2])) +
                   l.slpXbrn1km[1]*((slpXbrn1km-l.slpXbrn1km[2])/(l.slpXbrn1km[3]-l.slpXbrn1km[2]))
    ) - linPN
    mx.raw  <- exp(exponent)/densNorm
    hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
    out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
    return(out)
  }
