/*
** svn $Id: wcofs.h 585 2014-12-19 11:17:00Z kurapov $
*******************************************************************************
** Copyright (c) 2002-2012 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for wcofs
**
** Application flag:   WCOFS
*/

#define NONLINEAR
#define SOLVE3D
#define CURVGRID
#define MASKING
#define NONLIN_EOS
#define SALINITY
#define VAR_RHO_2D

#define UV_ADV
#define UV_COR
#define UV_U3HADVECTION
#define UV_C4VADVECTION
#define UV_QDRAG
#define UV_VIS2
/* move the following CPP into roms.in
#define TS_U3HADVECTION
#define TS_C4VADVECTION
*/
#define TS_DIF2

#define DJ_GRADPS
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define RI_SPLINES

#define ANA_BSFLUX
#define ANA_BTFLUX
/*#define ANA_RAIN
#define ANA_SSFLUX*/

#define BULK_FLUXES
#define LONGWAVE_OUT
#define SOLAR_SOURCE
#define EMINUSP

#define MIX_S_TS
#define MIX_S_UV

#define MY25_MIXING
#define N2S2_HORAVG
#define RADIATION_2D
#define LIMIT_STFLX_COOLING

#define ADD_FSOBC
#define ADD_M2OBC
#define SSH_TIDES
#define UV_TIDES

#define STATIONS
#define AVERAGES
#define HDF5

/* specify as needed in the build script
#define PERFECT_RESTART
#define HDF5
#define DEFLATE*/
