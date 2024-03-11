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
** Application flag:   WCOFS-DA
*/

#define NONLINEAR
#define SOLVE3D
#define CURVGRID
#define MASKING
#define NONLIN_EOS
#define SALINITY
#undef VAR_RHO_2D

#define UV_ADV
#define UV_COR
#define UV_U3HADVECTION
#define UV_C4VADVECTION
#define UV_QDRAG
#define UV_VIS2
#define TS_DIF2

#define DJ_GRADPS

#define ANA_BSFLUX
#define ANA_BTFLUX
#undef  ANA_SSFLUX
#undef  ANA_RAIN
#undef  ANA_STFLUX
#undef  ANA_SMFLUX

#define BULK_FLUXES
#define LONGWAVE_OUT
#define SOLAR_SOURCE
#define EMINUSP

#define MIX_S_TS
#define MIX_S_UV

#define MY25_MIXING
#define N2S2_HORAVG
#define RADIATION_2D

#define ADD_FSOBC
#define ADD_M2OBC
#define SSH_TIDES
#define UV_TIDES
/*
#define COLLECT_ALLREDUCE
#define REDUCE_ALLREDUCE
#define BOUNDARY_ALLREDUCE
*/
#define HDF5 
#undef  RAMP_TIDES
#undef  AVERAGES
#undef  PERFECT_RESTART

#define RBL4DVAR
#define REMOVE_LAPACK_GOTOS

/*
**-----------------------------------------------------------------------------
**  Variational Data Assimilation.
**-----------------------------------------------------------------------------
*/

/*
**  Options to compute error covariance normalization coefficients.
*/

#ifdef NORMALIZATION
# undef  ADJUST_BOUNDARY
# undef  ADJUST_WSTRESS
# undef  ADJUST_STFLUX
# define CORRELATION
# define VCONVOLUTION
# define IMPLICIT_VCONV
# define FULL_GRID
# define FORWARD_WRITE
# define FORWARD_READ
# define FORWARD_MIXING
# define OUT_DOUBLE
#endif

/*
**  Options for adjoint-based algorithms sanity checks.
*/

#ifdef SANITY_CHECK
# define FULL_GRID
# define FORWARD_READ
# define FORWARD_WRITE
# define FORWARD_MIXING
# define OUT_DOUBLE
# define ANA_PERTURB
# define ANA_INITIAL
#endif


/*
**  Common options to all 4DVAR algorithms.
*/

#if defined ARRAY_MODES || defined CLIPPING            || \
    defined RBL4DVAR     || defined RBL4DVAR_ANA_SENSITIVITY || \
    defined RBL4DVAR_FCT_SENSITIVITY
# undef  ADJUST_BOUNDARY
# undef  ADJUST_WSTRESS
# undef  ADJUST_STFLUX
# define VCONVOLUTION
# define IMPLICIT_VCONV
# undef  BALANCE_OPERATOR
# ifdef BALANCE_OPERATOR
#  define ZETA_ELLIPTIC
# endif
# define FORWARD_WRITE
# define FORWARD_READ
# define FORWARD_MIXING
# define FORWARD_FLUXES
# define PRIOR_BULK_FLUXES
# define OUT_DOUBLE
#endif

/*
**  Special options for each 4DVAR algorithm.
*/

#if defined RBL4DVAR
# define RPCG
#endif

#if defined ARRAY_MODES || \
    defined W4DVAR      || defined W4DVAR_SENSITIVITY
# define RPM_RELAXATION
#endif
