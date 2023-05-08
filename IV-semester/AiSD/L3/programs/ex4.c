#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "algorithms.h"

int main() {
    clock_t start, end;
    double cpu_time_used;
    int huge_array[100000];
    for (int i = 0; i < 100000; i++) {
        huge_array[i] = i;
    }
    double avg_comps_beginning;
    double avg_comps_middle;
    double avg_comps_end;
    double avg_comps_nowhere;
    double avg_comps_random;
    double avg_time_beginning;
    double avg_time_middle;
    double avg_time_end;
    double avg_time_nowhere;
    double avg_time_random;
    char* filename;
	FILE* fp;
	filename = "ex4.csv";
	fp = fopen(filename, "w");
	if (fp == NULL) {
        	printf("Error opening the file %s", filename);
        	return -1;
    }
    fprintf(fp, "type;position;n;time;comps\n");
    //Searching values that are "near" the beginning
    for (int n = 1000; n <= 100000; n += 1000) {
        int array[n];
        int searched_value;
        memcpy(array, &huge_array[0], n * sizeof(int));
        avg_comps_beginning = 0.0;
        avg_comps_middle = 0.0;
        avg_comps_end = 0.0;
        avg_comps_nowhere = 0.0;
        avg_comps_random = 0.0;
        avg_time_beginning = 0.0;
        avg_time_middle = 0.0;
        avg_time_end = 0.0;
        avg_time_nowhere = 0.0;
        avg_time_random = 0.0;
        for (int k = 1; k <= 50; k++) {
            //"Near" the beginning
            searched_value = rand() % (n / 100);
            start = clock();
            binarySearch(array, 0, n - 1, searched_value);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_beginning += cpu_time_used;
            avg_comps_beginning += comparisons;
            comparisons = 0;
            //In the "middle"
            searched_value = 4.95 * (n / 10) + rand() % (n / 100);
            start = clock();
            binarySearch(array, 0, n - 1, searched_value);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_middle += cpu_time_used;
            avg_comps_middle += comparisons;
            comparisons = 0;
            //In the "end"
            searched_value = 9.9 * (n / 10) + rand() % (n / 100);
            start = clock();
            binarySearch(array, 0, n - 1, searched_value);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_end += cpu_time_used;
            avg_comps_end += comparisons;
            comparisons = 0;
            //Not in the scope
            searched_value = -1;
            start = clock();
            binarySearch(array, 0, n - 1, searched_value);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_nowhere += cpu_time_used;
            avg_comps_nowhere += comparisons;
            comparisons = 0;
            //Random value
            searched_value = rand() % n;
            start = clock();
            binarySearch(array, 0, n - 1, searched_value);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_random += cpu_time_used;
            avg_comps_random += comparisons;
            comparisons = 0;
        }
        fprintf(fp, "normal;beginning;%d;%lf;%lf\n", n, (avg_time_beginning / 50.0) * 1000000, avg_comps_beginning / 50.0);
        fprintf(fp, "normal;middle;%d;%lf;%lf\n", n, (avg_time_middle / 50.0) * 1000000, avg_comps_middle / 50.0);
        fprintf(fp, "normal;end;%d;%lf;%lf\n", n, (avg_time_end / 50.0) * 1000000, avg_comps_end / 50.0);
        fprintf(fp, "normal;nowhere;%d;%lf;%lf\n", n, (avg_time_nowhere / 50.0) * 1000000, avg_comps_nowhere / 50.0);
        fprintf(fp, "normal;random;%d;%lf;%lf\n", n, (avg_time_random / 50.0) * 1000000, avg_comps_random / 50.0);
        fprintf(fp, "coefficient;beginning;%d;%lf;%lf\n", n, ((avg_time_beginning / log2(n))  / 50.0) * 1000000, (avg_comps_beginning / log2(n)) / 50.0);
        fprintf(fp, "coefficient;middle;%d;%lf;%lf\n", n, ((avg_time_middle / log2(n)) / 50.0) * 1000000, (avg_comps_middle / log2(n)) / 50.0);
        fprintf(fp, "coefficient;end;%d;%lf;%lf\n", n, ((avg_time_end / log2(n)) / 50.0) * 1000000, (avg_comps_end / log2(n)) / 50.0);
        fprintf(fp, "coefficient;nowhere;%d;%lf;%lf\n", n, ((avg_time_nowhere / log2(n)) / 50.0) * 1000000, (avg_comps_nowhere / log2(n)) / 50.0);
        fprintf(fp, "coefficient;random;%d;%lf;%lf\n", n, ((avg_time_random / log2(n)) / 50.0) * 1000000, (avg_comps_random / log2(n)) / 50.0);
    }
    fclose(fp);

    return 0;
}