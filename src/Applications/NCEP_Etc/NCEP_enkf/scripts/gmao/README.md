GEOS Ensemble Data Assimilation System
======================================

Sequence of calls:
------------------
1) setobsvr.csh             - set up observer
2) obsvr_ensemble.csh       - run observer (GSIsa.x-type)
3) analyses:
   3a) atmos_enkf.csh       - run AOD analysis (multiple opts)
   3b) atmos_enkf.csh       - run EnkF
4) post_eana                - post-ensemble analysis
   4a) atmens_stats.csh     - ensemble mean analysis
   4b) atmens_recenter.csh  - recenter analysis around central analysis (i.e., hybrid GSI)
5) atmos_ens2gcm.csh        - convert EnKF output to required IAU/RST files
6) gcm_ensemble.csh         - run ensemble of GCMs
7) atmens_arch.csh          - archives ensemble members and stats

Auxiliar functions:
------------------
AtmEnsConfig.csh         - env var settings

acquire_atmens.csh       - acquires existing ensemble; used for replay and obs. impact

atm_ens.j                - main job control script

atmens_stats.csh         - calculates ensemble statistics (mean/rms); called by ensemble scripts

dry_atm_ens.j            - runs dry-case (no-actual run, just stub)

jobgen.pl                - generates general pbs job script

jobmonitor.csh           - monitor present jobs in pbs

makeiau.csh              - explicit call to makeiau

update_ens.csh           - move ensemble to proper update location

Resource files handled in default settings of atmens_setup.pl
-------------------------------------------------------------

aens_stoch.rc        - settings related to stochastic physics tendencies (SPPT)

AGCM.rc.tmpl         - parameter settings controlling GCM options

AtmEnsConfig.csh     - configuration of EnADAS

atmens_storage.arc   - template names for various output entries

atmos_enkf.nml.tmpl  - namelists parameters for meterology EnKF

CAP.rc.tmpl          - model CAP parameters

dyn_recenter.rc:     - control recentering parameters

* dyn_recenter_l132.rc       - for L132 model w/o SPPT

* dyn_recenter_l132_sppt.rc  - for L132 model w/ SPPT

* dyn_recenter_l72.rc        - for L72 model w/o SPPT

* dyn_recenter_l72_sppt.rc   - for L72 model w/ SPPT

GAAS_GridComp.rc

GEOS_ChemGridComp.rc

GSI_GridComp_ensfinal.rc.tmpl  - final GSI observer (OmA) 

GSI_GridComp.rc.tmpl           - GSI observers (mean and members)

HISTAENS.rc.tmpl               - diagnostic output from model members

mkiau.rc.tmpl                  - IAU generation settings

mp_stats_perts.rc              - settings for program calculating mean NMC perturbations

mp_stats.rc                    - settings for program calculating ensemble mean

nmcperts.rc                    - settings for NMC perturbations

obs1gsi_mean.rc                - GSI namelists for mean observer

obs1gsi_member.rc              - GSI nemalists for member observers

odsstats_ktonly.rc             - list of KTs used to calculate summary of obs residual statitics

post_egcm.rc                   - controls what gets model output ensemble means/spread are calculated for


Resource files handling addition (non-default) options
------------------------------------------------------
aero_enkf_nml.tmpl   - 

ana_aodmean.rc

ana_aodmember.rc

atmens_berror.rc

atmens_efsens.rc

atmens_efsostorage.arc

atmens_incenergy.rc.tmpl

atmens_rst_regrid.rc

AtmOSEConfig.csh

atmos_enkf_sens.nml.tmpl

central_ana.rc

easyeana.rc

ene_adaptinf.rc

HISTAGEPS.rc.tmpl

HISTAOSE.rc.tmpl

obs_aodmean.rc

obs_aodmember.rc

First analysis runs:      <ens_bkg>+obs+bkg   -> CentralANA -> inc             *  
Then atmos-model integration can take place as usual:
                          inc + rst           -> GCM        -> bkg/rst         *  
Concurrently to usual DAS run we have:
 Observer runs:           <ens_bkg>+obs       -> obvr_ens   -> <diag>          v1
 Then EnkF runs:          <ens_bkg>+<diag>    -> enkf       -> <ens_ana>       v1
 Recentring takes place:  <ens_ana>+ana       -> recenter   -> <ens_ana>       v1
 Create increments:       <ens_bkg>+<ens_ana> -> makeiau    -> <ens_inc>       v1
 Finally run ens fcsts:   <ens_inc>+<rst>     -> GCM        -> <ens_bkg>       v1

Now that <ens_bkg> and bkg/rst are available we can go on to the next cycle
Remarks:
 a) in the status column above: (*) as is currently; (v1) means first version done;
    (tbd) means to-be-done
 b) at first we'll simplify matters a little and make: <rst>=rst; this makes
    cycling reall easy; all we have to cycle are the ensemble of bkg's, <ens_bkg>
 c) need minor changes to main DAS script so it picks up ensemble of bkgs for GSI
 d) must be careful to store initial rst's from central DAS to serve as triggers 
    for ensemble forecasts, that is, the rst on the lhs of central atmos-model
    integration above are the ones needed for running the ens fcsts.

Notation: <o> stands for ensemble (and mean stuff)
          inc stands for IAU increment


Possible sequence in a GEOSDAS cycle:
------------------------------------
I) 3dvar (IAU) context
I.1) all begins with the central hybrid-analysis: 
     - ensemble of "backgrounds" need to be available
       (therefore need to be recycled)
I.2) next run obsvr_ensemble.csh (gsi-observer)
I.3) followed by atmos_enkf.csh (EnKF)
>> at this stage we would have available the usual 
   IAU-inc for the central forecast, as well as 
   all IAU-inc for each member
I.4) run (low-res) ensemble forecasts:
     I.4a) simple setting: start fcst from same IC as hi-res
     I.4b) real thing: recycles mean fcst rst and starts from it
I.5) run central (hi-res) forecasts
back to (1)

New requirements:
 a) recyling of bkg.eta and bkg.sfc from ensemble fcsts
 b) archiving of ensemble bkg/ana means and bkg.eta/sfc for
    reproducibility 

II) 4dvar context
II.1) all begins w/ the central forecast

advantages:
  - consistent w/ 4dvar cycle, and therefore would easily accommodate 
    the hyb-4dvar case
  - in the 4dvar context we have to think as the model integration
    as part of the analysis (doesn't not take too long to do a 12 or
    24 hr fcst) to feed the inner loop 

disdvantages:
  - for ops the 4dvar cycle as I had it now is disadvantageous since 
    becomes difficult to time the run w/ the arrival of the observations
    (to do this properly would would need to split the gcm/gsi jobs)
  - in the 3dvar context we'd be needing to do another gcm to extend the forecast(s)
    or do a single run, but run a job underneath to monitory the arrival of the data
    and launching of the central analysis

The whole ordering of the cycling issue could be avoided if the scripts were split.


Tips for running within DAS
===========================

0) run fvsetup to setup a regular 3dvar/IAU experiment

1) initializing the ensemble:
   1a) create ensemble of backgrounds:
   run atm_ens.j with option to generate ensemble in desired 
   experiment look at env variable  GEN_ATM_ENS
   remarks:
     - that no other ensemble-related env var can be on
     - this will run the script called gen_ensemble.csh
     - notice you should not submit atm_ens.j to the pbs queue
       at this point, simply run it at command line
     - if you know more or less what this does, you might
       want to de-migrate the bkg files before running this
       procedure at command-line
   1b) create ensemble of restarts:
       you can use script gen_ensrst.csh for this purpose
      1b1) single resolution: simply copy all files w/
           name extension *.bin from the recycle directory
           to each of the member directories where gen_ensemble
           placed the backgrounds (step 1a, above)
      1b2) dual resolution: the is the most common case, that is,
           when the ensemble is ran at a coarser resolution than 
           the central cycle. For this, look for restarts at desired
           resolution that are at the same date/time as the initial
           date/time of the experiment. Then copy each *.bin file into
           the member directories where the bkg files reside, from
           step 1a above. Initial restarts could be converted (interpolated)
           using Larry Takacs script for rst conversion from the rst's
           in the recycle directory - in this case, you'll need to be careful
           w/ a bootstrap strategy (see below)
   1c) note that a bootstrap capability exists in ensemble mode. All that needs
       to be done if for a typical AGCM.BOOTSTRAP.rc.tmpl file to be placed
       together w/ the fully correct AGCM.rc.tmpl in the run/atmens directory.
       Once all members of the ensemble have been bootstrapped the bootstrap
       rc file will be moved out of the way and the usual AGCM.rc.tmpl will 
       become the file used.
   
2) in main g5das.j script:

   setenv NSEGS     1
   setenv HYBRIDGSI $FVHOME/atmens

   remarks:
     - for now, hybrid can only run in 6-hr segments
     - make sure obsclass in atm_ens.j is the same as that in g5das.j
     - it might be a good idea to let the whole thing cycle for a day
       or so with beta1_inv=1.00 in the gsi.rc.tmpl file; this is to
       initialize the ensemble to something reasonable before it then
       start affecting the central, hybrid, analysis.

3) edit gsi.rc.tmpl in run directory and set hybrid namelist to proper resolution, 
   number of members, etc

4) for now: copy etc directory under 
     src/Applications/NCEP_Etc/NCEP_enkf/scripts/gmao
   into a directory under the run directory named atmens
     mkdir      $FVHOME/run/atmens
     cp xxx/etc $FVHOME/run/atmens
   edit the resource files there to make sure resolution matches resolution
   of desired ensemble.
   remarks:
      - ideally, there resolution of the files in run/atmens would be 
        consistent w/ what is in gsi.rc.tmpl in (3) above. However, 
        until the GMAO version of the dual resolution GSI is functioning
        this is not the case; the resolution in gsi.rc.tmpl of the ens
        should be that of the central analysis.

5) notice that each individual procedure of the atmos-ensemble has one
   or more hidden files that get created inside the work directory to 
   tell the script what has and has not finished. In principle, if the
   job does not complete in time, the ens work directory will be preserved.
   The user can then edit atm_ens.j and fix the ens work directory so the
   resubmitted job can pick up from where it left. Remember to reset the 
   ens work directory to its general settings in atm_ens.j once the job 
   has completed, so that other cycles can start afresh. 

6) to run from pre-existing ensemble place file atmens_replay.acq
   in run directory of experiment with location of existing ensemble.
   See NCEP_Etc/NCEP_enkf/scripts/gmao/etc/atmens_replay.acq for an example.
   Notice that, at this time, no checks are done for consistency in number 
   of ensemble members, that is, pre-existing ensemble better have enough members.

7) a similar mechanism to the replay mode above allows running the adjoint GSI
   using the hybrid option. In this case, the file atmens_asens.acq controls 
   the location of the ensemble to be used when the adj GSI runs. Notice the
   gsi_asens.rc.tmpl needs to be consistent with gsi.rc.tmpl in regards to the
   options related to the ensemble.

TO BE DONE
==========
A) note that atm_ens.j can start running as soon as central analysis completes
   indeed all the member-observers can run concurrently to central analysis
   One could add knobs to fvpsas/g54var to launch:
    1a) atm_ens.j for observer-only right before it starts running central 
        analysis
    2b) once both central ana and ens-observers are complete atm_ens can 
        start running enkf and gcm-members
   These would need a little coordination and a fixed enswork directory defined
   from with the full DAS job script

B) Decide what logs to save (particularly the observer logs - it would be 
   good to save the Jo tables)

C) Add option to run ensemble-only DAS, that is, without a central GSI analysis

D) Building a COFFEE (along the lines of Heemink et al. 2001):
   COFFEE stands for the complementary orthogonal-subspace filter for efficient ensemble
          (clearly the acronym is messed up; it doesn't matter - the idea is good
          COFEE would be more correct)
   d.1) modify GSI to allow minization to get dominant eigenmodes of B:
        d.1.1) use a no-obs situation, and modify rhs so that one can solve for:
                 B^{-1} dx = dx
               with Lanczos CG.
        d.1.2) tell Lanczos to get bottom part of spectrum (instead of top part)
        d.1.3) if steps (d.1.1) and (d.1.2) cannot be easily done, then one could
               construct an offline code that applies B to a vector and build the 
               eigendecomp from there. This would be a great opportunity to 
               make the application of B more modular in GSI.
   d.2) write a program that will read the pre-computed dominant modes of B, 
        for a given resolution, and project the ensemble forecast members
        onto the orthogonal subspace spanned by the dominant eigenvectors of B.
        That is, if Pi is the projection onto the dominant eigenmodes of B, then
              Ef(COFEE) = [ x_1 - xf + e_1, ..., x_n - xf + e_n ]
        where e_i = N(0,(I-Pi)*B(I-Pi)'), where x_i are the ensemble members and 
        xbar their mean, and xf is the central forecast.
   d.3) instead of feeding Ef=[x_1-xbar,...,x_n-xbar] to the hybrid GSI, 
        as normally done in the EnKF, we would feed Ef(COFFE)-generated ensemble.
        >> I think with this one could get rid of the beta1_inv parameter, couldn't we?
        (perhaps not just because the projector will only be approximate)

E) Variations on EnGSI:
   In this first implementation of the EnGSI (Nov 2011) we have each member being
   generated after a full GSI analysis (with single outer loop, for simplicity). 
   This is rather expensive approach without much basis to guarantee indepence 
   of the members (likely to have issues of collapse of the ensemble - though one
   can also perturb the obs to prevent this from happening).
   Another variant of this could simply run GSI for the mean state, use Lanczos-CG
   for thi, with single outer loop, and request the code to put as many members
   as desired by doing:
        x(i) = sqrt(Pa) random_vector(i) 
   where random_vector(i)=N(0,I).
   The script can then easily take care of linking these vectors to the member 
   directories w/ proper names, so all else would go on merrily.

