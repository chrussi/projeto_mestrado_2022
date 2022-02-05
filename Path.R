library(lavaan)
library(adespatial)

# importar arquivo 'data.csv'
df=read.csv(file.choose(),sep=',')
names(df)

# remove comunidades com riqueza=0
df2=df[rowSums(df[,c('X1','X2','X3','X4','X5')])>0,]

########## matrizes de distância/medias
#composição
sp.d=dist.ldc(df2[,c('X1','X2','X3','X4','X5')],method = 'sorensen',binary = T)

#delta geografica
geo.d=dist(df2[,c('x','y')])

#delta temporal
temporal.d=dist(df2$campanha)

#delta ambiente
tas.d=dist(df2$tas)#temperatura
prec.d=dist(df2$pr)#precipitacao
fcover.d=dist(df2$fcover)

# tamanho médio
df2$tam=rowSums(df2[,c('X1','X2','X3','X4','X5')])

size.m=do.call(cbind,lapply(1:nrow(df2), function(x){
  unlist(lapply(1:nrow(df2), function(y){
    (df2[x,'tam']+
       df2[y,'tam'])/2
  }))
}))

size.m=as.dist(size.m)

#conectividade media
con.m=do.call(cbind,lapply(1:nrow(df2), function(x){
  unlist(lapply(1:nrow(df2), function(y){
    (df2[x,'connect']+
       df2[y,'connect'])/2
  }))
}))

con.m=as.dist(con.m)

#clima medio
#temperatura
tas.m=do.call(cbind,lapply(1:nrow(df2), function(x){
  unlist(lapply(1:nrow(df2), function(y){
    (df2[x,'tas']+
       df2[y,'tas'])/2
  }))
}))

tas.m=as.dist(tas.m)

#precipitacao
prec.m=do.call(cbind,lapply(1:nrow(df2), function(x){
  unlist(lapply(1:nrow(df2), function(y){
    (df2[x,'pr']+
       df2[y,'pr'])/2
  }))
}))

prec.m=as.dist(prec.m)

#cobertura florestal
fcover.m=do.call(cbind,lapply(1:nrow(df2), function(x){
  unlist(lapply(1:nrow(df2), function(y){
    (df2[x,'fcover']+
       df2[y,'fcover'])/2
  }))
}))

fcover.m=as.dist(fcover.m)

################ formatando matriz de dados para analises posteriotes
data.d=list('Y'=as.matrix(sp.d),
               'geo.d'=as.matrix(geo.d),
               'temporal.d'=as.matrix(temporal.d),
               'tas.d'=as.matrix(tas.d),
               'prec.d'=as.matrix(prec.d),
               'fcover.d'=as.matrix(fcover.d),
               'size.m'=as.matrix(size.m),
               'con.m'=as.matrix(con.m),
               'tas.m'=as.matrix(tas.m),
               'prec.m'=as.matrix(prec.m),
              'fcover.m'=as.matrix(fcover.m))

#somente pares espaciais, o resto vira NA
spatial.data=lapply(data.d, function(x){
  x[data.d$temporal.d>0]=NA
  return(x)
})

#somente pares temporais, o resto vira NA
temporal.data=lapply(data.d, function(x){
  x[data.d$geo>0]=NA
  return(x)
})

############ análise de caminhos da dimensão espacial
model<-'Y ~ tas.d + prec.d + fcover.d + size.m + con.m + geo.d
            size.m ~ con.m + tas.m + prec.m + fcover.m
            prec.d ~ geo.d
            tas.d ~ geo.d'


perm=permutation.based.pathanalysis(model,spatial.data,#spatial.data
                                    nperm = 999,verb=F)
perm

############ análise de caminhos da dimensão temporal
model<-'Y ~ tas.d + prec.d + fcover.d + size.m + con.m + temporal.d
            size.m ~ con.m + tas.m + prec.m + fcover.m
            prec.d ~ temporal.d
            tas.d ~ temporal.d'


perm=permutation.based.pathanalysis(model,temporal.data,#temporal.data
                                    nperm = 999,verb=F)
perm
