set scheme tufte
lgraph vote_share year if family_name == "Right-wing", xti("Right-wing") yti("Vote share") name(g1, replace) nodraw
lgraph vote_share year if family_name == "Communist/Socialist", xti("Communist/Socialist") yti("Vote share") name(g2, replace) nodraw
graph combine g1 g2, ycommon name(combined, replace)
