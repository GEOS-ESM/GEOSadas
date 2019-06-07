CopyResourceFiles_(){

    # global variables: FVHOME, FVWORK
    # vED is expected to be in path

    local ftypes ft rcfile rcfilename target vars

    # QUESTION: Do we still have *.sed files??
    ftypes=(\
	        *.arc\               # archiving rules
	        *.acq\               # acquiring rules
	        *.rc\                # resource files
	        *.namelist\          # Fortran namelists
	        # *.sed\             # templates
	        *.tbl\               # fvCCM diagnostic table
	        *.tmpl\              # any templates
	        prepobs*\            # all parameter files
	        gmao_acft_bias.parm\
	            )
    for ft in "${ftypes[@]}"
    do
	    /bin/cp $FVHOME/run/$ft $FVWORK
    done
    if [ -d $FVHOME/run/fsens ]; then
	    /bin/cp $FVHOME/run/fsens/*.tmpl $FVWORK
	    /bin/cp $FVHOME/run/fsens/*.rc $FVWORK
    fi

    cat fvcore_layout.rc >> input.nml

    # Disble GAAS feedback from GCM, by default
    vED -i $FVWORK/GEOS_ChemGridComp.rc -vv ENABLE_GAAS=.FALSE.
    cat $FVWORK/GEOS_ChemGridComp.rc

    # Get GOCART rc files
    if [ -d "$FVHOME/run/gocart" ]; then
	    cp $FVHOME/run/gocart/* $FVWORK
    fi

    # Edit CARMAchem_Registry.rc file
    rcfile=$FVWORK/CARMAchem_Registry.rc
    if [ -e $rcfile ]; then
	    vars="DU_OPTICS SS_OPTICS BC_OPTICS SM_OPTICS"
	    subst_path.pl -i $rcfile fvInput $FVHOME/fvInput $vars
    fi

}
