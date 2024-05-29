/*
** svn $Id: upwelling.h 709 2014-01-23 20:09:38Z arango $
*******************************************************************************
** Copyright (c) 2002-2014 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Gulf of Maine Operational Forecast System (GoMOFS) 
**
** Application flag:   GoMOFS
** Input script:       nos.gomofs.roms.in
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
#define TS_A4HADVECTION
#define TS_A4VADVECTION
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
#define UV_TIDES              /* flag - tidal currents */
#define ADD_FSOBC       /* add tidal elevation to processed OBC data */
#define ADD_M2OBC        /* tidal currents  to processed OBC data */
#undef  FSOBC_REDUCED   /* for computing press gradient; applied to the cases of _M2FLATHER, _M2REDUCED */

#define GLS_MIXING 
#undef MY25_MIXING

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
#define LONGWAVE_OUT
#define SOLAR_SOURCE
#define EMINUSP
#define HDF5
/*# define ANA_CLOUD
# define ANA_RAIN */
#else
# define ANA_SMFLUX
# define ANA_STFLUX
#endif

#define STATIONS

#define LIMIT_STFLX_COOLING

#define WET_DRY
#define LIMIT_BSTRESS
#undef LIMIT_VDIFF
#undef LIMIT_VVISC
 
#undef RAMP_TIDES      
#define NO_LBC_ATT

