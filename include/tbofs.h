/*
** svn $Id: upwelling.h 709 2014-01-23 20:09:38Z arango $
*******************************************************************************
** Copyright (c) 2002-2014 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Tampa Bay Operational Forecast System (TBOFS) 
**
** Application flag:   TBOFS
** Input script:       nos.tbofs.roms.in
*/

#define SOLVE3D
#define WET_DRY
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
#define UV_LOGDRAG

#define UV_U3HADVECTION
#define UV_C4VADVECTION
/* move the following CPP into roms.in
#define TS_U3HADVECTION
#define TS_C4VADVECTION

#undef TS_A4HADVECTION
#undef TS_A4VADVECTION
*/
#define UV_VIS2    /*undefine based on Rutgers recommendation from 2016 ROMS Workshop */
#define TS_DIF2
#define MIX_S_UV
#define MIX_GEO_TS
#define VISC_GRID  /*undefine based on Rutgers recommendation from 2016 ROMS Workshop */
#define DIFF_GRID
#define DJ_GRADPS
#define RADIATION_2D

#define SSH_TIDES
#define UV_TIDES
#define ADD_FSOBC
#define ADD_M2OBC

#undef GLS_MIXING 
#define MY25_MIXING

#if defined MY25_MIXING || defined GLS_MIXING
# define N2S2_HORAVG
# define KANTHA_CLAYSON
#endif
#ifdef PERFECT_RESTART
# undef OUT_DOUBLE
#endif

                                                                               
#define ANA_BTFLUX

#define BULK_FLUXES
#ifdef BULK_FLUXES
# define LONGWAVE_OUT
# define ANA_CLOUD
# define ANA_RAIN
#else
# define ANA_SMFLUX
# define ANA_STFLUX
#endif
#define STATIONS
#define NO_LBC_ATT
#define SOLAR_SOURCE
#define EMINUSP
#define HDF5
/*
#define LIMIT_VDIFF
#define LIMIT_VVISC
*/
/* #define LIMIT_STFLX_COOLING 
#define RAMP_TIDES
#define NO_LBC_ATT  */  
