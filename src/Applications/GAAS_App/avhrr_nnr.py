"""
This module implements the evaluation of AVHRR Neural Net Retrievals (NNR)
based on PATMOS-X data. 

"""

import warnings
from pyobs         import avhrr, sknet
from numpy         import  c_ as cat
from numpy         import  copy, ones, sin, cos, exp, arccos, pi, any, log

MISSING = -1.0e20
d2r = pi / 180.

class AVHRR_NNR(avhrr.AVHRR_L2B):
    """
    This class extends the AVHRR Level 2B class by adding NNR evaluation
    methods.
    """

    def __init__(self, Path='/nobackup/AVHRR/Level2/NPZ/2008/*_00?.npz',
                 N_bal=None,Verb=False):

        self.verbose = Verb
        self.ident = 'avhrr'
        self.surface = 'ocean'
        
        avhrr.AVHRR_L2B.__init__(self,Path,Verb=Verb)

        # Balance Observing system
        # ------------------------
        if N_bal is not None:
            I = self.balance(N_bal)
            self.reduce(I)

        # Air mass factor
        # ---------------
        self.amf = (1./cos(d2r*self.SolarZenith))+(1./cos(d2r*self.SensorZenith))  

        # Glint Angle
        # -----------
        RelativeAzimuth = self.SensorAzimuth # = anchor_relative_azimuth
        cosGlintAngle = cos(self.SolarZenith*d2r) * cos(self.SensorZenith*d2r) + \
                        sin(self.SolarZenith*d2r) * sin(self.SensorZenith*d2r) * \
                        cos(RelativeAzimuth*d2r)

        # Angle transforms: for NN calculations we work with cosine of angles
        # -------------------------------------------------------------------
        self.SensorAzimuth = cos(self.SensorAzimuth*d2r)   
        self.SensorZenith  = cos(self.SensorZenith*d2r)
        self.SolarAzimuth  = cos(self.SolarAzimuth*d2r)
        self.SolarZenith   = cos(self.SolarZenith*d2r)
        self.GlintAngle    = cosGlintAngle

        # Sanity check
        # ------------
        self.iValid = (self.tau_630  > -0.01) &\
                      (self.ref_630 >  0)    &\
                      (self.ref_860 >  0) 
#                     (self.tau_860  > -0.01) &\

        # Log transforms
        # --------------
        self.ltau_630 = log(self.tau_630+0.01)
#       self.ltau_860 = log(self.tau_860+0.01)
        self.lref_630 = log(self.ref_630)
        self.lref_860 = log(self.ref_860)
                 
    def _getInputs(self):
        """
        Get Inputs for Neural Net.
        """

        # Loop over inputs
        # ----------------
        first = True
        for inputName in self.net.InputNames:

            if self.verbose>0:
                print('Getting NN input ',inputName)

            # Retrieve input
            # --------------
            input = self.__dict__[inputName][:]
            self.iValid = self.iValid & (input!=MISSING) # Q/C

            # Concatenate Inputs
            # ------------------
            if first:
                inputs = input
                first = False
            else:
                inputs = cat[inputs,input]

        # Keep only good observations
        # ---------------------------
        return inputs[self.iValid,:]

#--
    def apply(self,nnFile='/nobackup/NNR/Net/nnr_001b.avhrr_Tau.net'):
        """
        Evaluates NN retrieval.
        """

        # Load Network
        # ------------
        self.net = sknet.loadnet(nnFile)
                      
        # Stop here is no good obs available
        # ----------------------------------
        if self.nobs == 0:
            return # no data to work with
        if any(self.iValid) == False:
            return # no good data to work with

        if len(self.net.TargetNames)>1:
            raise ValueError('Strange, more than one predictor')

        # Evaluate NN on inputs
        # ---------------------
        targets = self.net(self._getInputs())

        name = self.net.TargetNames[0]
        if self.verbose>0:
            print("Evaluating NNR for <%s> with Log-AOD = "%name, self.net.laod) 

        # Output is always AOD
        # --------------------
        if self.net.laod:
            result = exp(targets) - 0.01 # inverse

        else:
            result = targets

        # Set retrieved values, possibly with UNDEFS
        # ------------------------------------------
        self.__dict__[name] = MISSING * ones(self.lon.shape)
        self.channels_ = [550.,] # channels being retrieved
        self.__dict__[name][self.iValid] = result.ravel()

        return result

#---
        
    __call__= apply

#---

if __name__ == "__main__":

    a = AVHRR_NNR(Verb=True)

    aod = a.apply()
