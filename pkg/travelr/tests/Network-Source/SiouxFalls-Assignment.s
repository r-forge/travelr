; Cube script to load SiouxFalls network with various strategies.

RUN PGM=NETWORK
FILEI LINKI='SiouxFalls_Network.csv' VAR=A,B,CAPACITY,DISTANCE,FFTIME,BETA,POWER,SPEED,TOLL,TYPE START=(RECORD!='From,To,Capacity,Length,FFTime,B,Power,Speed,Toll,Type')
FILEI NODEI='SiouxFalls_Nodes.csv' VAR=N,X,Y START=(RECORD!='Node,X,y')
FILEO NETO='SiouxFalls_Cube.net'
ZONES=24
ENDRUN

RUN PGM=MATRIX
FILEI MATI=SiouxFalls_OD_Cube.csv FIELDS=#1,#2,#3-26 PATTERN=IJ:V
FILEO MATO=SiouxFalls_OD.mat MO=1
ZONES=24
FILLMW MW[1]=MI.1.1
ENDRUN

RUN PGM=HIGHWAY

NETI='SiouxFalls_Cube.net'
MATI='SiouxFalls_OD.mat'
NETO='SiouxFalls_CubeLoaded.net'
PRINTO='SiouxFalls_PathTrace.csv'

PARAMETERS,
   COMBINE=AVE,
   MAXITERS=1

PHASE=LINKREAD
  LINKCLASS=1
  T0=LI.FFTIME
  T1=T0
ENDPHASE

PHASE=ILOOP
  PATHLOAD PATH=LI.FFTIME VOL[1]=MI.1.1 THRUNODE=1,
           TRACE=(I=1-24 & J=1-24),
             PRINTO=1 CSV=T,
             FORM=4L LIST=I,J,A,B,
             FORM=8.2L LIST=LI.FFTIME,_PATHCOST
ENDPHASE

ENDRUN

RUN PGM=NETWORK
LINKI='SiouxFalls_CubeLoaded.net'
LINKO='SiouxFalls_LoadTarget.dbf' EXCLUDE=CAPACITY,DISTANCE,BETA,POWER,TOLL,TYPE
ENDRUN
