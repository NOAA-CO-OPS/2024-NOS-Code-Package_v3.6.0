#include "parmetis.h"

void wrap_parmetis_v3_partgeomkway_(idxtype *vtxdist, idxtype *xadj, idxtype *adjncy,
              idxtype *vwgt, idxtype *adjwgt, int *wgtflag, int *numflag, int *ndims,
              float *xyz, int *ncon, int *nparts, float *tpwgts, float *ubvec,
              int *options, int *edgecut, idxtype *part, MPI_Fint *fcomm) {

 MPI_Comm ccomm;
 ccomm = MPI_Comm_f2c(*fcomm);

 ParMETIS_V3_PartGeomKway(vtxdist, xadj, adjncy,
              vwgt, adjwgt, wgtflag, numflag, ndims,
              xyz, ncon, nparts, tpwgts, ubvec,
              options, edgecut, part, &ccomm);

}




