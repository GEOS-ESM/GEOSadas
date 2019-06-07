"""
This module implements the MODIS NNR AOD retrievals.

This version works from MODIS MOD04/MYD04 Level 2 files.

"""
import os, sys
import warnings
from   pyobs.mxd04 import MxD04_L2, MISSING, granules, BEST 
from   ffnet       import loadnet
from   numpy       import c_ as cat
from   numpy       import copy, ones, sin, cos, exp, arccos, pi, any, log
import numpy       as     np
from   pyobs.bits  import BITS

# SDS to be read in
# ------------
SDS = dict( META =    ( "Scan_Start_Time",
                        "Latitude",
                        "Longitude",
                        "Solar_Zenith",
                        "Solar_Azimuth",
                        "Sensor_Zenith",
                        "Sensor_Azimuth",
                        "Scattering_Angle",
                        "Glint_Angle"),

            LAND =    ( 'Corrected_Optical_Depth_Land',
                        'Mean_Reflectance_Land',
                        'Surface_Reflectance_Land',
                        'Cloud_Fraction_Land',
                        'Quality_Assurance_Land',
                        'Deep_Blue_Cloud_Fraction_Land'),

            OCEAN =   ( 'Effective_Optical_Depth_Best_Ocean',
                        'Mean_Reflectance_Ocean',
                        'Cloud_Fraction_Ocean',               
                        'Quality_Assurance_Ocean'),

            DEEP =    ( 'Deep_Blue_Aerosol_Optical_Depth_550_Land',
                        'Deep_Blue_Spectral_Aerosol_Optical_Depth_Land',
                        'Deep_Blue_Spectral_TOA_Reflectance_Land',
                        'Deep_Blue_Spectral_Surface_Reflectance_Land',
                        'Deep_Blue_Cloud_Fraction_Land',
                        'Deep_Blue_Aerosol_Optical_Depth_550_Land_QA_Flag',
                        'Mean_Reflectance_Land',
                        'Surface_Reflectance_Land',
                        'Aerosol_Cloud_Fraction_Land',
                        'Quality_Assurance_Land'))

ALIAS = dict( Deep_Blue_Aerosol_Optical_Depth_550_Land = 'aod550',
              Mean_Reflectance_Land = 'reflectance_lnd',
              Surface_Reflectance_Land = 'sfc_reflectance_lnd',
              Aerosol_Cloud_Fraction_Land = 'cloud_lnd',
              Quality_Assurance_Land = 'qa_lnd' )

# Channels for TOA reflectance 
# -----------------------------
CHANNELS  = dict (
                   LAND  = ( 470, 550, 660, 870, 1200, 1600, 2100, 412, 440),
                   OCEAN = ( 470, 550, 660, 870, 1200, 1600, 2100 ),
                   DEEP  = ( 412, 470, 660 ),
                 )

SCHANNELS = dict (
                   LAND = ( 470, 660, 2100 ),
                   DEEP = ( 412, 470, 660 ),
                )


# Translate Inputs between NNR and MODIS classes
# -----------------------------------------------
TranslateInput = dict ( mRef412  = ('reflectance',412),
                        mRef440  = ('reflectance',440),
                        mRef470  = ('reflectance',470),
                        mRef550  = ('reflectance',550),
                        mRef660  = ('reflectance',660),
                        mRef870  = ('reflectance',870),
                        mRef1200 = ('reflectance',1200),
                        mRef1600 = ('reflectance',1600),
                        mRef2100 = ('reflectance',2100),
                        mSre412  = ('sfc_reflectance',412),
                        mSre470  = ('sfc_reflectance',470),
                        mSre660  = ('sfc_reflectance',660),
                        mSre2100 = ('sfc_reflectance',2100),                                    
                      )

for var in ( 'ScatteringAngle','GlintAngle',
             'SolarAzimuth', 'SolarZenith',
             'SensorAzimuth','SensorZenith',
             'cloud','qa_flag'  ):
    TranslateInput[var] = (var,)

# Translate Targets between ANET and MODIS classes
# ------------------------------------------------
TranslateTarget = dict ( aTau440 = ( 'aod_', 440 ),
                         aTau470 = ( 'aod_', 470 ),
                         aTau500 = ( 'aod_', 500 ),
                         aTau550 = ( 'aod_', 550 ),
                         aTau660 = ( 'aod_', 660 ),
                         aTau870 = ( 'aod_', 870 ),
                         )

class MxD04_NNR(MxD04_L2):
    """
    This class extends MODIS by adding methods for producing
    NNR AOD retrievals based on the Neural Net model developed
    with class *abc_c6*.
    """

    def __init__(self,l2_path,prod,algo,syn_time,aer_x,
                 cloud_thresh=0.70,
                 glint_thresh=40.0,
                 scat_thresh=170.0,
                 cloudFree=None,
                 aodmax=1.0,
                 coll='006',verbose=0):
        """
        Contructs a MXD04 object from MODIS Aerosol Level 2
        granules. On input,

         l2_path --- top directory for the MODIS Level 2 files;
                      it must have subdirectories MOD04 and MYD04.
            prod --- either *MOD04* (Terra) or *MYD04* (Aqua)
            algo --- aerosol algorithm: LAND, OCEAN or DEEP (for
                      Deep Blue)
        syn_time --- synoptic time

        cloud_tresh --- cloud fraction treshhold
        cloudFree   --- cloud fraction threshhold for assuring no cloud contaminations when aod is > aodmax
                        if None, no cloud free check is made
              
        The following attributes are also defined:
           fractions dust, sea salt, BC+OC, sulfate
           aod_coarse
           wind
           
        It also performs Q/C, setting attribute iGood. On,
        input, *cloud_thresh* is the cloud fraction limit.
        When DEEP BLUE algorithm is requested, filters for 
        land retrievals where DARK TARGET obs are unavailable.
        """

        self.verbose = verbose
        self.algo    = algo
        self.cloudFree = cloudFree
        self.aodmax = aodmax
        
        # Initialize superclass
        # ---------------------
        Files = granules(l2_path,prod,syn_time,coll=coll)
        if algo != "DEEP":
            MxD04_L2.__init__(self,Files,algo,syn_time,
                              only_good=True,
                              SDS=SDS,
                              alias={'Deep_Blue_Cloud_Fraction_Land':'cloud_deep'},
                              Verb=verbose)            
        else:        
            MxD04_L2.__init__(self,Files,algo,syn_time,
                              only_good=False,
                              SDS=SDS,                            
                              alias=ALIAS,
                              Verb=verbose)

        if self.nobs < 1:
            return # no obs, nothing to do

        # Reorganize Reflectance Arrays
        # -----------------------------
        self.rChannels = CHANNELS[algo]
        if algo in SCHANNELS:
            self.sChannels = SCHANNELS[algo]

        if algo == "OCEAN":
            self.reflectance = self.reflectance[:,0:7]  #not using 412, 443, and 745 for now
        if algo == "LAND":
            self.reflectance = self.reflectance[:,0:-1]  #not using 745 for now

        # 3-Ch Algorithm only used when Dark Target data is unavailable
        # --------------------------------------------------------------

        if algo == "DEEP":
            # Get DARK TARGET qa_flag
            self.qa_flag_lnd = BITS(self.Quality_Assurance_Land[:,0])[1:4]            
            lndGood = self.qa_flag_lnd == BEST
            lndGood = lndGood & (self.cloud_lnd < cloud_thresh)
            rChannels = CHANNELS["LAND"]
            sChannels = SCHANNELS["LAND"]
            for i,c in enumerate(rChannels):
                lndGood = lndGood & (self.reflectance_lnd[:,i]>0)

            for i,c in enumerate(sChannels):
                lndGood = lndGood & (self.sfc_reflectance_lnd[:,i]>0)

            self.iGood = (self.qa_flag == BEST) & ~lndGood

            # Keep only "good" observations
            # -----------------------------
            m = self.iGood
            for sds in self.SDS:
                rank = len(self.__dict__[sds].shape)
                if rank == 1:
                    self.__dict__[sds] = self.__dict__[sds][m]
                elif rank == 2:
                    self.__dict__[sds] = self.__dict__[sds][m,:]
                else:
                    raise IndexError, 'invalid rank=%d'%rank

            # Reset aliases
            for sds in self.SDS:
                if sds in self.ALIAS:
                    self.__dict__[self.ALIAS[sds]] = self.__dict__[sds] 


            self.qa_flag = self.qa_flag[m]
            self.aod     = self.aod[m,:]
            self.time    = self.time[m]
            self.Time    = self.Time[m]
            self.iGood   = self.iGood[m] 
            self.nobs    = self.Longitude.shape[0]         

            if self.nobs < 1:
                return # no obs, nothing to do             


        # Q/C
        # ---      
        self.iGood = self.cloud<cloud_thresh  
        if algo == "LAND":
            self.iGood = self.iGood & (self.cloud_deep<cloud_thresh)
        elif algo == "DEEP":
            self.iGood = self.iGood & (self.cloud_lnd<cloud_thresh)

        for i,c in enumerate(self.rChannels):
            self.iGood = self.iGood & (self.reflectance[:,i]>0)

        if algo in SCHANNELS:
            for i,c in enumerate(self.sChannels):
                self.iGood = self.iGood & (self.sfc_reflectance[:,i]>0)

        if algo == "OCEAN":
            self.iGood = self.iGood & (self.GlintAngle > glint_thresh)

        if algo != "OCEAN":
            self.iGood = self.iGood & (self.ScatteringAngle < scat_thresh)

        if any(self.iGood) == False:
            print "WARNING: Strange, no good obs left to work with"
            return

        # Create attribute for holding NNR predicted AOD
        # ----------------------------------------------
        self.aod_ = MISSING * ones((self.nobs,len(self.channels)))

        # Make sure same good AOD is kept for gridding
        # --------------------------------------------
        if len(self.aod.shape) == 1:
            self.aod.shape = self.aod.shape + (1,)
        self.aod[self.iGood==False,:] = MISSING


        # Angle transforms: for NN calculations we work with cosine of angles
        # -------------------------------------------------------------------
        self.ScatteringAngle = cos(self.ScatteringAngle*pi/180.0) 
        self.SensorAzimuth   = cos(self.SensorAzimuth*pi/180.0)   
        self.SensorZenith    = cos(self.SensorZenith*pi/180.0)    
        self.SolarAzimuth    = cos(self.SolarAzimuth*pi/180.0)    
        self.SolarZenith     = cos(self.SolarZenith*pi/180.0)     
        self.GlintAngle      = cos(self.GlintAngle*pi/180.0)

        # Get fractional composition
        # ------------------------------
        self.speciate(aer_x,Verbose=verbose)


    def speciate(self,aer_x,Verbose=False):
        """
        Use GAAS to derive fractional composition.
        """

        self.sampleFile(aer_x,onlyVars=('TOTEXTTAU',
                                        'DUEXTTAU',
                                        'SSEXTTAU',
                                        'BCEXTTAU',
                                        'OCEXTTAU',
                                        'SUEXTTAU',
                                        ),Verbose=Verbose)

        s = self.sample
        I = (s.TOTEXTTAU<=0)
        s.TOTEXTTAU[I] = 1.E30
        self.fdu  = s.DUEXTTAU / s.TOTEXTTAU
        self.fss  = s.SSEXTTAU / s.TOTEXTTAU
        self.fbc  = s.BCEXTTAU / s.TOTEXTTAU
        self.foc  = s.OCEXTTAU / s.TOTEXTTAU
        self.fcc  = self.fbc + self.foc
        self.fsu  = s.SUEXTTAU / s.TOTEXTTAU

        # Special handle nitrate (treat it as it were sulfate)
        # ----------------------------------------------------
        try:
            self.sampleFile(aer_x,onlyVars=('NIEXTTAU',),Verbose=Verbose)
            self.fsu += self.sample.NIEXTTAU / s.TOTEXTTAU
        except:
            pass   # ignore it for systems without nitrates

        del self.sample

#---
    def sampleFile(self, inFile, npzFile=None, onlyVars=None, Verbose=False):
        """
        Interpolates all variables of inFile and optionally
        save them to file *npzFile*
        """
        from gfio import GFIO, GFIOctl, GFIOHandle

        # Instantiate grads and open file
        # -------------------------------
        name, ext = os.path.splitext(inFile)
        if ext in ( '.nc4', '.nc', '.hdf'):
          fh = GFIO(inFile)     # open single file
          if fh.lm == 1:
            timeInterp = False    # no time interpolation in this case
          else:
            raise ValueError, "cannot handle files with more tha 1 time, use ctl instead"
        else:
          fh = GFIOctl(inFile)  # open timeseries
          timeInterp = True     # perform time interpolation
          tymes = np.array([self.syn_time]*self.nobs)

        self.sample = GFIOHandle(inFile)
        if onlyVars is None:
            onlyVars = fh.vname

        lons = self.lon
        lats = self.lat

        

        # Loop over variables on file
        # ---------------------------
        for v in onlyVars:
            if Verbose:
                print "<> Sampling ", v
            if timeInterp:
              var = fh.sample(v,lons,lats,tymes,Verbose=Verbose)
            else:
              var = fh.interp(v,lons,lats)
            if (var.size == 1) & (len(var.shape) == 0):
                var.shape = (1,)  #protect against when only one value is returned and shape=()
            if len(var.shape) == 1:
                self.sample.__dict__[v] = var
            elif len(var.shape) == 2:
                var = var.T # shape should be (nobs,nz)
                self.sample.__dict__[v] = var
            else:
                raise IndexError, 'variable <%s> has rank = %d'%(v,len(var.shape))

        if npzFile is not None:
            savez(npzFile,**self.sample.__dict__)            


    def _loadNet(self,nnFile):
        """
        Loads the Neural Net weights created with class ABC.
        """
        self.net = loadnet(nnFile)

    def _getInputs(self):
        """
        Get Inputs for Neural Net.
        """

        # Loop over inputs
        # ----------------
        first = True
        for inputName in self.net.InputNames:
            try:
                iName = TranslateInput[inputName]
            except:
                iName = inputName

            if self.verbose>0:
                print 'Getting NN input ',iName

            # Retrieve input
            # --------------
            if type(iName) is str:
                input = self.__dict__[iName][:]

            elif len(iName) == 2:
                name, ch = iName
                if 'mSre' in inputName: # LAND or DEEP, surface reflectivity
                    k = list(self.sChannels).index(ch) # index of channel 
                elif 'mRef' in inputName: # MOD04 reflectances
                    k = list(self.rChannels).index(ch) # index of channel 

                input = self.__dict__[name][:,k]
                
            elif len(iName) == 1:
                name = iName[0]
                input = self.__dict__[name][:]
                
            else:
                raise ValueError, "strange, len(iName)=%d"%len(iName)

            # Concatenate Inputs
            # ------------------
            if first:
                inputs = input
                first = False
            else:
                inputs = cat[inputs,input]

        # Keep only good observations
        # ---------------------------
        return inputs[self.iGood,:]

#--
    def apply(self,nnFile):
        """
        Apply bias correction to AOD.
        """

        # Stop here is no good obs available
        # ----------------------------------
        if self.nobs == 0:
            return # no data to work with
        if any(self.iGood) == False:
            return # no good data to work with

        # Load the Neural Net
        # -------------------
        self._loadNet(nnFile)

        # Evaluate NN on inputs
        # ---------------------
        targets = self.net(self._getInputs())


        # Targets do not have to be in MODIS retrieval
        # ----------------------------------------------
        for i,targetName in enumerate(self.net.TargetNames):
            name, ch = TranslateTarget[targetName]
            try:
                k = list(self.channels).index(ch) # index of channel            
            except:
                # add new target channel to end
                self.channels += (ch,)
                self.aod  = np.append(self.aod,MISSING*ones((self.nobs,1)),axis=1)
                self.aod_ = np.append(self.aod_,MISSING*ones((self.nobs,1)),axis=1)

        # Replace targets with unbiased values
        # ------------------------------------
        self.channels_ = [] # channels being revised
        for i,targetName in enumerate(self.net.TargetNames):
            name, ch = TranslateTarget[targetName]
            if self.verbose>0:
                if self.net.laod:
                    print "NN Retrieving log(AOD+0.01) at %dnm "%ch
                else:
                    print "NN Retrieving AOD at %dnm "%ch
            k = list(self.channels).index(ch) # index of channel            
            self.channels_ = self.channels_ + [ch,]
            if self.net.laod:
                result = exp(targets[:,i]) - 0.01 # inverse
            else:
                result = targets[:,i]

            self.__dict__[name][self.iGood,k] = result


        # Do extra cloud filtering if required
        if self.cloudFree is not None:                 
            if self.algo == "LAND":
                cloudy = (self.cloud_deep>=self.cloudFree) & (self.cloud>=self.cloudFree)
            elif self.algo == "DEEP":
                cloudy = (self.cloud_lnd>=self.cloudFree) & (self.cloud>=self.cloudFree)
            elif self.algo == "OCEAN":
                cloudy = (self.cloud>=self.cloudFree)
    
            contaminated = np.zeros(np.sum(self.iGood)).astype(bool)
            for targetName in self.net.TargetNames:
                name, ch = TranslateTarget[targetName]
                k = list(self.channels).index(ch) # index of channel
                result = self.__dict__[name][self.iGood,k]
                contaminated = contaminated | ( (result > self.aodmax) & cloudy[self.iGood] )
                
            for targetName in self.net.TargetNames:
                name, ch = TranslateTarget[targetName]
                k = list(self.channels).index(ch) # index of channel
                self.__dict__[name][self.iGood,k][contaminated] = MISSING

            self.iGood[self.iGood][contaminated] = False





#---
        
    __call__= apply


#---

if __name__ == "__main__":

    from datetime import datetime

    l2_path = '/nobackup/MODIS/Level2/'
    algo    = 'DEEP'
    prod    = 'MOD04'
    coll    = '006'
    aer_x   = '/nobackup/NNR/Misc/tavg1_2d_aer_Nx'

    syn_time = datetime(2008,6,30,12,0,0)
    syn_time = datetime(2016,12,19,15,0,0)

    if algo == 'OCEAN':
        nn_file = '/nobackup/NNR/Net/nnr_003.mydo_Tau.net'
    elif algo == 'LAND':
        nn_file = '/nobackup/NNR/Net/nnr_003.mydl_Tau.net'
    elif algo == 'DEEP':
        nn_file = '/nobackup/NNR/Net/nnr_003.mydd_Tau.net'

    m = MxD04_NNR(l2_path,prod,algo.upper(),syn_time,aer_x,
                  coll=coll,
                  cloud_thresh=0.7,
                  verbose=True)

    m.apply(nn_file)
    aod = m.aod_
