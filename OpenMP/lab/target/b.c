#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 102400
int main(int argc, char **argv)
{
	int i;
	float p[N]  = {0};
	float v1[N] = {0};
	float v2[N] = {0};

#pragma omp target map(tofrom: v1, v2, p)
#pragma omp parallel for
 for (i=0; i<N; i++) {
	 p[i] = 0.0;
	 v1[i] = 1.0;
	 v2[i] = 2.0;
 }

#pragma omp target map(to: v1, v2) map(from: p)
#pragma omp parallel for
 for (i=0; i<N; i++)
	 p[i] = v1[i] * v2[i];

printf("%g\t%g\t%g\n",p[0],v1[0],v2[0]);
printf("%g\t%g\t%g\n",p[N-1],v1[N-1],v2[N-1]);
}

