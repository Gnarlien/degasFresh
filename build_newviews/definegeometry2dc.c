

#define REAL double  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "triangle.h"   

void report(io,markers,reporttriangles,reportneighbors,reportsegments,
reportedges,reportnorms,reportholes)
struct triangulateio*io;
int markers;
int reporttriangles;
int reportneighbors;
int reportsegments;
int reportedges;
int reportnorms;
int reportholes;
{
int i,j;

for(i= 0;i<io->numberofpoints;i++){
printf("Point %4d:",i);
for(j= 0;j<2;j++){
printf("  %.6g",io->pointlist[i*2+j]);
}
if(io->numberofpointattributes>0){
printf("   attributes");
}
for(j= 0;j<io->numberofpointattributes;j++){
printf("  %.6g",
io->pointattributelist[i*io->numberofpointattributes+j]);
}
if(markers){
printf("   marker %d\n",io->pointmarkerlist[i]);
}else{
printf("\n");
}
}
printf("\n");

if(reporttriangles||reportneighbors){
for(i= 0;i<io->numberoftriangles;i++){
if(reporttriangles){
printf("Triangle %4d points:",i);
for(j= 0;j<io->numberofcorners;j++){
printf("  %4d",io->trianglelist[i*io->numberofcorners+j]);
}
if(io->numberoftriangleattributes>0){
printf("   attributes");
}
for(j= 0;j<io->numberoftriangleattributes;j++){
printf("  %.6g",io->triangleattributelist[i*
io->numberoftriangleattributes+j]);
}
printf("\n");
}
if(reportneighbors){
printf("Triangle %4d neighbors:",i);
for(j= 0;j<3;j++){
printf("  %4d",io->neighborlist[i*3+j]);
}
printf("\n");
}
}
printf("\n");
}

if(reportsegments){
for(i= 0;i<io->numberofsegments;i++){
printf("Segment %4d points:",i);
for(j= 0;j<2;j++){
printf("  %4d",io->segmentlist[i*2+j]);
}
if(markers){
printf("   marker %d\n",io->segmentmarkerlist[i]);
}else{
printf("\n");
}
}
printf("\n");
}

if(reportedges){
for(i= 0;i<io->numberofedges;i++){
printf("Edge %4d points:",i);
for(j= 0;j<2;j++){
printf("  %4d",io->edgelist[i*2+j]);
}
if(reportnorms&&(io->edgelist[i*2+1]==-1)){
for(j= 0;j<2;j++){
printf("  %.6g",io->normlist[i*2+j]);
}
}
if(markers){
printf("   marker %d\n",io->edgemarkerlist[i]);
}else{
printf("\n");
}
}
printf("\n");
}
if(reportholes){
for(i= 0;i<io->numberofholes;i++){
printf("Hole %4d points:",i);
for(j= 0;j<2;j++){
printf("  %.6g",io->holelist[i*2+j]);
}
}
printf("\n");
}
}


void poly2triangles_(npoints,polygon,area,nholes,hole,refine,ntriangles,triangles,markers)

int*npoints;
int*nholes;
int*refine;
double*area;
double(*polygon)[2];
double(*hole)[2];

int*ntriangles;
double(*triangles)[4][2];
int(*markers)[4];
{
struct triangulateio in,mid,out;
struct triangulateio*final;
int i,j,n,nh,pt,jt,k,idup,nunq,ip,munq;
int pointmap[200000],pointinvmap[200000];
int segmap[200000][2];

n= *npoints;




nunq= 0;
for(i= 0;i<n;++i){
idup= 0;
if(i>0){
for(j= 0;j<i;++j){
if((polygon[i][0]==polygon[j][0])
&&(polygon[i][1]==polygon[j][1])){
pointmap[i]= pointmap[j];
idup= 1;
}
}
}
if(idup==0){
pointmap[i]= nunq;
pointinvmap[nunq]= i;
nunq++;
}
}





munq= 0;
for(i= 0;i<n;++i){





if(i==n-1){
ip= 0;
}
else{
ip= i+1;
}
if(pointmap[i]==pointmap[ip]){
idup= 1;
}
else{
idup= 0;
if(munq>0){
for(j= 0;j<munq;++j){
if(((pointmap[i]==segmap[j][0])&&(pointmap[ip]==segmap[j][1]))
||(((pointmap[i]==segmap[j][1])&&(pointmap[ip]==segmap[j][0])))){
idup= 1;
}
}
}
}
if(idup==0){
segmap[munq][0]= pointmap[i];
segmap[munq][1]= pointmap[ip];
munq++;
}
}



in.numberofpoints= nunq;
in.numberofpointattributes= 0;
in.pointlist= (double*)malloc(in.numberofpoints*2*sizeof(double));
in.pointmarkerlist= (int*)malloc(in.numberofpoints*sizeof(int));
j= 0;
for(i= 0;i<nunq;++i){
in.pointlist[j]= polygon[pointinvmap[i]][0];
j++;
in.pointlist[j]= polygon[pointinvmap[i]][1];
j++;






in.pointmarkerlist[i]= pointinvmap[i]+1;
}
in.numberofsegments= munq;
in.segmentlist= (int*)malloc(in.numberofsegments*2*sizeof(int));
j= 0;
for(i= 0;i<munq;++i){
in.segmentlist[j]= segmap[i][0];
j++;
in.segmentlist[j]= segmap[i][1];
j++;
}


nh= *nholes;
in.numberofholes= nh;
if(nh>0){
in.holelist= (double*)malloc(in.numberofholes*2*sizeof(double));
j= -1;
for(i= 0;i<nh;++i){
j++;
in.holelist[j]= hole[i][0];
j++;
in.holelist[j]= hole[i][1];
}
}
else{
in.holelist= (double*)NULL;
}
in.numberofregions= 0;
in.numberoftriangles= 0;
in.regionlist= (double*)NULL;
in.segmentmarkerlist= (int*)NULL;
mid.pointlist= (double*)NULL;
mid.pointmarkerlist= (int*)NULL;
mid.trianglelist= (int*)NULL;
mid.segmentlist= (int*)NULL;
mid.segmentmarkerlist= (int*)NULL;
mid.edgelist= (int*)NULL;
mid.edgemarkerlist= (int*)NULL;
mid.normlist= (double*)NULL;

triangulate("pzQ",&in,&mid,(struct triangulateio*)NULL);


out.pointlist= (double*)NULL;
out.pointmarkerlist= (int*)NULL;
out.trianglelist= (int*)NULL;
out.segmentlist= (int*)NULL;
out.segmentmarkerlist= (int*)NULL;
out.edgelist= (int*)NULL;
out.edgemarkerlist= (int*)NULL;
out.normlist= (double*)NULL;

if(*refine==1){
char args1[16]= "rpqa";
char args2[]= "zYQ";
char areastring[9];
sprintf(areastring,"%8.6f",*area);
strcat(args1,areastring);
strcat(args1,args2);
triangulate(args1,&mid,&out,(struct triangulateio*)NULL);


final= &out;
}
else{
final= &mid;
}

if(final->numberofcorners!=3){
printf("STOP!!! Triangle thinks triangles have %i corners",final->numberofcorners);
}
if(final->numberoftriangles>100000-1){
printf("STOP!!! Number of triangles %i exceeds dimensions of triangles array!",final->numberoftriangles);
}

for(i= 0;i<final->numberoftriangles;++i){
for(j= 0;j<final->numberofcorners;++j){
pt= final->trianglelist[i*final->numberofcorners+j];
jt= final->numberofcorners-j;
triangles[i][jt][0]= final->pointlist[2*pt];
triangles[i][jt][1]= final->pointlist[2*pt+1];



markers[i][jt]= final->pointmarkerlist[pt]-1;
}
triangles[i][0][0]= triangles[i][3][0];
triangles[i][0][1]= triangles[i][3][1];
markers[i][0]= markers[i][3];
}
*ntriangles= final->numberoftriangles;

free(in.pointlist);
free(in.segmentlist);
free(in.pointmarkerlist);
free(in.regionlist);
free(in.segmentmarkerlist);
free(mid.pointlist);
free(mid.segmentlist);
free(mid.pointmarkerlist);
free(mid.trianglelist);
free(mid.segmentmarkerlist);
free(mid.edgelist);
free(mid.edgemarkerlist);
free(mid.normlist);
free(out.pointlist);
free(out.segmentlist);
free(out.pointmarkerlist);
free(out.trianglelist);
free(out.segmentmarkerlist);
free(out.edgelist);
free(out.edgemarkerlist);
free(out.normlist);
if(nh>0){
free(in.holelist);
}
}




