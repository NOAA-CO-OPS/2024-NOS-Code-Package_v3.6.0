/*
** svn $Id: upwelling.h 1099 2022-01-06 21:01:01Z arango $
*******************************************************************************
** Copyright (c) 2002-2022 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Chesapeake Bay Operational Forecast System (CBOFS) 
**
** Application flag:   CBOFS
** Input script:       nos.cbofs.roms.in
*/

#define SOLVE3D
#define PERFECT_RESTART
#define ATM_PRESS
#define CURVGRID
#define MASKING
#define SALINITY
#ifdef SALINITY
# define ANA_BSFLUX
# define ANA_SSFLUX
#endif

#define NONLIN_EOS
 
#define UV_ADV
#define UV_COR
#define UV_QDRAG

#define UV_U3HADVECTION
#define UV_C4VADVECTION
/* move the following CPP into roms.in
#define TS_U3HADVECTION
#define TS_C4VADVECTION
*/
#define DJ_GRADPS

#define UV_VIS2 
#define TS_DIF2
#define MIX_S_UV
#define MIX_GEO_TS
#define VISC_GRID 
#define DIFF_GRID


#define RADIATION_2D

#define SSH_TIDES
#define UV_TIDES
#define ADD_FSOBC
#define ADD_M2OBC

#define GLS_MIXING 
#undef MY25_MIXING

#if defined MY25_MIXING || defined GLS_MIXING
# define N2S2_HORAVG
# define KANTHA_CLAYSON
#endif
#ifdef PERFECT_RESTART
# undef OUT_DOUBLE
#endif
/*
* The following part is added to include the 1-Term Dissolved Oxygen
* simulation.  This is added by L. Zheng on 11/10/2016
*/
#define HYPOXIA_SRM
#define SURFACE_DO_SATURATION
#define ANA_SPFLUX
#define ANA_BPFLUX
                                                                               
#define ANA_BTFLUX

#define BULK_FLUXES
#ifdef BULK_FLUXES
#define LONGWAVE_OUT
#define ANA_CLOUD
#define ANA_RAIN
#else
#define ANA_SMFLUX
#define ANA_STFLUX
#endif

#define SOLAR_SOURCE
#define EMINUSP
#define HDF5

#define STATIONS
#define LIMIT_STFLX_COOLING
#undef LIMIT_VDIFF
#undef LIMIT_VVISC
#define NO_LBC_ATT
/*
#define RAMP_TIDES
#define NO_LBC_ATT */  
