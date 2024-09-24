
install.packages('rsconnect')


rsconnect::setAccountInfo(name='trm-jpj',
                          token='16EFF5AD38FFC8C2E7F6306059D68051',
                          secret='DE15GmpAv1KDY0VQc44XNGv3km/SiGUgdS0DapjB')
library(rsconnect)
rsconnect::deployApp('S:/CKA/Git/Ledelsesinformation/lastbiler')
