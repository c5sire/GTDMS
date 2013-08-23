
if(!has.data(fb$PPE)  & length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
  PPE 	= (NPE*100)/NTP	})			

if(!has.data(fb$PPH)  & length(fb$NTP)>0 & length(fb$PPH)>0 ) fb=within(fb,{	
  PPH 	= (NPH*100)/NTP	})			
if(!has.data(fb$TNTP) & length(fb$NMTCI)>0 & length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{	
  TNTP	= NMTCI + NMTCII + NNoMTP	})			
if(!has.data(fb$TNTP) & length(fb$NMTP)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{	
  TNTP	= NMTP+ NNoMTP})			


if(!has.data(fb$TNTPL) & length(fb$TNTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  TNTPL	= TNTP/NPH	})			
if(!has.data(fb$NMTP) & length(fb$NMTCI)>0 & length(fb$NMTCII)>0  ) fb=within(fb,{	
  NMTP	= NMTCI + NMTCII	})			
if(!has.data(fb$NMTPL) & length(fb$NMTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  NMTPL	= NMTP/NPH	})			
if(!has.data(fb$TTWP) & length(fb$MTWCI)>0 & length(fb$MTWCII)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{	
  TTWP	= MTWCI + MTWCII + NoMTWP	})	
if(!has.data(fb$TTWP) & length(fb$MTWP)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{	
  TTWP	= MTWP + NoMTWP	})	

if(!has.data(fb$TTWPL) & length(fb$TTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  TTWPL	= TTWP/NPH	})			
if(!has.data(fb$TTYNA) & length(fb$TTWP)>0  ) fb=within(fb,{	
  TTYNA	= TTWP/plot.size*10	})	#GTDM-39		
if(!has.data(fb$TTYA) & length(fb$TTWPL)>0  ) fb=within(fb,{	
  TTYA	= TTWPL*plant.den/1000}) # GTDM-45			
if(!has.data(fb$MTWP) & length(fb$MTWCI)>0 & length(fb$MTWCII)>0 ) fb=within(fb,{	
  MTWP	= MTWCI + MTWCII})			
if(!has.data(fb$MTWPL) & length(fb$MTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  MTWPL	= MTWP/NPH})			
if(!has.data(fb$MTYNA) & length(fb$MTWP)>0 ) fb=within(fb,{	
  MTYNA	= MTWP/plot.size*10})	#GTDM-39		
if(!has.data(fb$MTYA) & length(fb$MTWPL)>0 ) fb=within(fb,{	
  MTYA	= MTWPL*plant.den/1000})#GTDM-39			
if(!has.data(fb$ATW) & length(fb$TTWP)>0 & length(fb$TNTP)>0) fb=within(fb,{	
  ATW		= TTWP/TNTP*1000})			
if(!has.data(fb$ATMW) & length(fb$MTWP)>0 & length(fb$NMTP)>0) fb=within(fb,{	
  ATMW	= MTWP/NMTP*1000})			
if(!has.data(fb$DM1) & length(fb$DWTS1)>0 & length(fb$FWTS1)>0) fb=within(fb,{	
  DM1		= DWTS1/FWTS1 * 100})			
if(!has.data(fb$DM2) & length(fb$DWTS2)>0 & length(fb$FWTS2)>0) fb=within(fb,{	
  DM2		= DWTS2/FWTS2 * 100})			
if(!has.data(fb$AVDM) & length(fb$DM1)>0 & length(fb$DM2)>0) fb=within(fb,{	
  AVDM	= (DM1 + DM2)/2})			
if(!has.data(fb$SG) & length(fb$TWA)>0 & length(fb$TWA)>0 & length(fb$TWW)>0) fb=within(fb,{	
  SG		= TWA/(TWA-TWW)})			
if(!has.data(fb$OCS1) & length(fb$IWS1)>0 & length(fb$FWS1)>0 ) fb=within(fb,{	
  OCS1	= 100 - ((IWS1/FWS1)*100)})			
if(!has.data(fb$OCS2) & length(fb$IWS2)>0 & length(fb$FWS2)>0 ) fb=within(fb,{	
  OCS2	= 100 - ((IWS2/FWS2)*100)})			
if(!has.data(fb$AOCP) & length(fb$OCS1)>0 & length(fb$OCS2)>0 ) fb=within(fb,{	
  AOCP	= (OCS1 + OCS2)/2})			


# dormancy variables
if(!has.data(fb$AVLGLATSP) & length(fb$LGLAT1)>0 & length(fb$LGLAT2)>0 & length(fb$LGLAT3)>0
   & length(fb$LGLAT4)>0 & length(fb$LGLAT5)>0 & length(fb$LGLAT6)>0) fb=within(fb,{  
     AVLGLATSP = mean(LGLAT1 + LGLAT2 + LGLAT3+ LGLAT4+ LGLAT5 + LGLAT6)
   })
if(!has.data(fb$AVTHSP) & length(fb$THLSP1)>0 & length(fb$THLSP2)>0) fb=within(fb,{  
  AVTHSP = mean(THLSP1 + THLSP2)
})
if(!has.data(fb$PW_USPT) & length(fb$ITW)>0 & length(fb$INTW)>0) fb=within(fb,{  
  PW_USPT = ((ITW-INTW)/ITW)*100
})
if(!has.data(fb$PW_SPT) & length(fb$ITW)>0 & length(fb$FTW)>0) fb=within(fb,{  
  PW_SPT = ((ITW-FTW)/ITW)*100
})

############### end dormancy

###############################################################################
# Start related variables for sweetpotato

if(!has.data(fb$TRW) & length(fb$CRW)>0 & length(fb$NCRW)>0 ) fb=within(fb,{	
  TRW	= apply(cbind(CRW,NCRW), 1, sum, na.rm=T)})

if(!has.data(fb$CYTHA) & length(fb$CRW)>0) fb=within(fb,{	
  CYTHA	= CRW*10/plot.size})

if(!has.data(fb$RYTHA) & length(fb$CRW)>0 & length(fb$NCRW)>0) fb=within(fb,{	
  RYTHA	= apply(cbind(CRW,NCRW), 1, sum, na.rm=T)*10/plot.size})

if(!has.data(fb$ACRW) & length(fb$CRW)>0 & length(fb$NOCR)>0) fb=within(fb,{	
  ACRW	= CRW/NOCR})

if(!has.data(fb$NRPP) & length(fb$NOCR)>0 & length(fb$NONC)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  NRPP	= apply(cbind(NOCR,NONC), 1, sum, na.rm=T)/NOPH})

if(!has.data(fb$YPP) & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  YPP	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)/NOPH})

if(!has.data(fb$CI) & length(fb$NOCR)>0 & length(fb$NONC)>0) fb=within(fb,{	
  CI	= NOCR/apply(cbind(NOCR,NONC), 1, sum, na.rm=T)*100})

if(!has.data(fb$HI) & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  HI	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)/apply(cbind(VW, CRW, NCRW), 1, sum, na.rm=T)*100})

if(!has.data(fb$SHI) & length(fb$NOPH)>0 & length(fb$NOPS)>0) fb=within(fb,{	
  SHI	= NOPH/NOPS*100})

if(!has.data(fb$BIOM) & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  BIOM	= apply(cbind(VW, CRW, NCRW), 1, sum, na.rm=T)*10/plot.size})

if(!has.data(fb$FYTHA) & length(fb$VW)>0) fb=within(fb,{	
  FYTHA	= VW*10/plot.size})

if(!has.data(fb$DM) & length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  DM	= DMD/DMF*100})

if(!has.data(fb$DMFY) & length(fb$VW)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  DMFY	= VW*10/plot.size*DMVD/DMVF})

if(!has.data(fb$DMRY) & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  DMRY	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)*10/plot.size*DMD/DMF})

if(!has.data(fb$RFR) & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  RFR	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)*(DMD/DMF)/(VW*DMVD/DMVF)})



# End related variables for sweetpotato
###############################################################################
