#define _POSIX_C_SOURCE 200809L
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 102400
int main(int argc, char **argv)
{
	int i;

	float *p = NULL;
	float *v1 = NULL;
	float *v2 = NULL;

	p = _mm_malloc(N*sizeof(float), 64);
	v1 = _mm_malloc(N*sizeof(float), 64);
	v2 = _mm_malloc(N*sizeof(float), 64);

#if 0
	float p[N];
	float v1[N];
	float v2[N];
#endif

#pragma omp target \
	map(tofrom: v1[0:N], v2[0:N], p[0:N])
#pragma omp parallel for
 for (i=0; i<N; i++) {
	 p[i] = 0.0;
	 v1[i] = 1.0;
	 v2[i] = 2.0;
 }

#if 0
#pragma omp target map(to: v1, v2) map(from: p)
#endif
#pragma omp parallel for
 for (i=0; i<N; i++)
	 p[i] = v1[i] * v2[i];

printf("%g\t%g\t%g\n",p[0],v1[0],v2[0]);
printf("%g\t%g\t%g\n",p[N-1],v1[N-1],v2[N-1]);
}

