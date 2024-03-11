#!/bin/bash
. /lfs/h1/nos/nosofs/noscrub/aijun.zhang/packages/nosofs.v3.7.2/versions/run.ver
module purge
module load envvar/${envvars_ver:?}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
export LSFDIR=/lfs/h1/nos/nosofs/noscrub/aijun.zhang/packages/nosofs.v3.7.2/pbs 
rm -f /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2/wcofs_*_03.out
rm -f /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2/wcofs_*_03.err
PREP=$(qsub $LSFDIR/jnos_wcofs_prep_03.pbs) 
qsub -W depend=afterok:$PREP $LSFDIR/jnos_wcofs_nowcst_fcst_03.pbs 
