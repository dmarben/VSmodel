#PARAMETERS from
#Evans, M. N., B. K. Reichert, A. Kaplan, K. J. Anchukaitis, E. A. Vaganov, M. K. Hughes, and M. A. Cane (2006),
#A forward modeling approach to paleoclimatic interpretation of tree-ring data, J. Geophys. Res., 111, G03008

Tmin=5
Topt1=18
Topt2=24
Tmax=31
ft1=1    #growth rate (slope>0) between Tmin and Topt1
ctg=1    #constant growth rate between Topt1 and Topt2
ft2=1    #growth rate (slope<0) between Topt2 and Tmax

Wmin=0.04
Wopt1=0.2
Wopt2=0.8
Wmax=0.9
fw1=1    #growth rate (slope>0) between Wmin and Wopt1
cwg=1    #constant growth rate between Wopt1 and Wopt2
fw2=1    #growth rate (slope<0) between Wopt2 and Wmax


Tbeg = 60 #temperature sum for initiation of growth (ºC)
tbeg = 10 #time period for temperature sum (days)
lr = 1000 #depth of root system (mm)


Pmax= 20  #maximum daily precip. for saturated soil (mm/day)
c1=0.72   #fraction of precipitation penetrating soil (not caught by crown) (rel. unit)
c2=0.12 #first coefficient for calculation of transpiration (mm/day) 
c3=0.175 # second coefficient for calculation of transpiration (1/degrees Celsius)
c4=0.001   #coefficient for water drainage from soil (dimensionless)

#Variables
# Ti temperature of a given day or month on a given year
# P precipitation of a given time step (day or month) on a given year
# Wi soil moisture of a given time step (day or month) on a given year

fP=min(c1*P,Pmax)  #function of daily precipitation,
E=c2*gt*exp(c3*Ti) #Daily transpiration
Q=c4*W             #daily runoff

# balance equation for soil water dynamics [Thornthwaite and Mather,1955; Alisov, 1956],
dW=fP-E-Q          #daily change in soil water content

# Temp function
Tfun<-function(Ti,...){
   if(Ti<=Tmin|Ti>=Tmax)(gt=0)                                    #no growth below Tmin or above Tmax
   if(Ti>Tmin&Ti<Topt1)(gt=ft1*((Ti-Tmin)/(Topt1-Tmin)))          #linear increasing growth between Tmin and Topt1
   if(Ti>=Topt1&Ti<=Topt2)(gt=ctg)                                #constant growth between Topt1 and Topt2
   if(Ti>Topt2&Ti<Tmax)(gt=ft2*((Ti-Tmax)/(Topt2-Tmax)))         #linear decreasing growth between Topt2 and Tmax
  return(gt)
  }

# Soil Moisture function
Wfun<-function(Wi,...){
  if(Wi<=Wmin|Wi>=Wmax)(gw=0)                                    #no growth below Wmin or above Wmax
  if(Wi>Wmin&Wi<Wopt1)(gw=fw1*((Wi-Wmin)/(Wopt1-Wmin)))          #linear increasing growth between Wmin and Wopt1
  if(Wi>=Wopt1&Wi<=Wopt2)(gw=cwg)                                #constant growth between Wopt1 and Wopt2
  if(Wi>Wopt2&Wi<Wmax)(gw=fw2*((Wi-Wmax)/(Wopt2-Wmax)))         #linear decreasing growth between Wopt2 and Wmax
  return(gw)
}


ww<-NULL
for( i in seq(0,1,0.01)){
  ww=rbind(ww,Wfun(i))
}
plot(ww, type="o")

tt<-NULL
for( i in seq(0,50,0.01)){
  tt=rbind(tt,Tfun(i))
}
plot(tt~seq(0,50,0.01), type="o")


#Cambial model

Vcr = 0.04   #minimum cambial cell growth rate (um/day)
Do = 4       #initial cambial cell size (um)
Dcr = 8      #cell size at which mitotic cycle begins (um)
Vm = 1       #growth rate during mitotic cycle (um/day)
Dm = 10      #cambial cell size at which mitosis occurs (um)


Vit=V