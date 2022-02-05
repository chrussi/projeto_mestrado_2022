library(adespatial)

# importae arquivo 'data.csv'
df=read.csv(file.choose(),sep=',')
names(df)

# calcula BD.espacial para cada campanha
BD.espacial=do.call(rbind,lapply(unique(df$campanha), function(x){
  
  m=df[df$campanha==x,]
  m.sp=m[,c('X1','X2','X3','X4','X5')]
  m.sp=m.sp[rowSums(m.sp)>0,]#comunidades com riqueza = 0 devem ser removidas
  beta.div.comp(m.sp,coef = 'BS')$part[1:3]#1=beta total, 2-turnover, 3=aninhamento
}))

# calcula BD.temporal para cada campanha
BD.temporal=do.call(rbind,lapply(unique(df$local), function(x){
  
  m=df[df$local==x,]
  m.sp=m[,c('X1','X2','X3','X4','X5')]
  m.sp=m.sp[rowSums(m.sp)>0,]#comunidades com riqueza = 0 devem ser removidas
  beta.div.comp(m.sp,coef = 'BS')$part[1:3]#1=beta total, 2-turnover, 3=aninhamento
}))

# teste t beta total
t=t.test(BD.espacial[,1],BD.temporal[,1])
c(t$statistic,t$p.value) # valor de t e de p

# teste t turnover
t2=t.test(BD.espacial[,2],BD.temporal[,2])
c(t2$statistic,t2$p.value) # valor de t e de p

# teste t aninhamento
t3=t.test(BD.espacial[,3],BD.temporal[,3])
c(t3$statistic,t3$p.value) # valor de t e de p

