#!/bin/bash
. /lfs/h1/nos/nosofs/noscrub/aijun.zhang/packages/nosofs.v3.7.2/versions/run.ver
module load envvar/${envvars_ver:?}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
if [ ! -d /lfs/h1/nos/ptmp/aijun.zhang/execlog/v3.7.2 ]; then 
   mkdir -p /lfs/h1/nos/ptmp/aijun.zhang/execlog/v3.7.2 
fi 
if [ ! -d /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2 ]; then 
   mkdir -p /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2 
fi 
rm -f /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2/sscofs_*_09.out
rm -f /lfs/h1/nos/ptmp/aijun.zhang/rpt/v3.7.2/sscofs_*_09.err
export LSFDIR=/lfs/h1/nos/nosofs/noscrub/aijun.zhang/packages/nosofs.v3.7.2/pbs 
PREP=$(qsub  $LSFDIR/jnos_sscofs_prep_09.pbs) 
NFRUN=$(qsub -W depend=afterok:$PREP $LSFDIR/jnos_sscofs_nowcst_fcst_09.pbs)
qsub -W depend=afterok:$NFRUN $LSFDIR/jnos_sscofs_aws_09.pbs
