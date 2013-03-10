/**@file generator.cpp
 * @brief main file for the Bidimensional Cutting Stock Problem generator *.dat files
 * @author Alberto De Bortoli
 */

#include <iostream>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

int main(int args, char ** argv)
{	
	srand(time(NULL));
	
	FILE * fpdata = fopen(argv[1],"w");
	if ( fpdata == NULL ) {
		printf("ERROR writing %s", argv[1]);
		exit(-1);
	}
	fprintf(fpdata,"param zero := 0.0 ;\n");

	int pieces = 10 + rand()%10;
		
   	if (args >=3 && argv[2][0] == '-' && argv[2][1] == 'i') {
		fprintf(fpdata,"param rect_width := %d.0 ;\n", 10 + rand()%10);
		fprintf(fpdata,"param rect_length := %d.0 ;\n", 10 + rand()%10);
		fprintf(fpdata,"param numPieces := %d ;\n", pieces);
		
		fprintf(fpdata,"param WIDTHS: LENGTHS: ORDERS :=\n");
		for (int i = 0; i<pieces; i++) {
			fprintf(fpdata,"          %d.0", 1 + rand()%10);
			fprintf(fpdata,"    %d.0", 1 + rand()%10);
			fprintf(fpdata,"    %d\n", 1 + rand()%10);
		}
		printf("Generated %s with integer values.\n", argv[1]);
	}
	else {
		fprintf(fpdata,"param rect_width := %d.%d ;\n", 10 + rand()%10, rand()%10);
		fprintf(fpdata,"param rect_length := %d.%d ;\n", 10 + rand()%10, rand()%10);
		fprintf(fpdata,"param numPieces := %d ;\n", pieces);
		
		fprintf(fpdata,"param WIDTHS: LENGTHS: ORDERS :=\n");
		for (int i = 0; i<pieces; i++) {
			fprintf(fpdata,"          %d.%d", 1 + rand()%10, rand()%10);
			fprintf(fpdata,"    %d.%d", 1 + rand()%10, rand()%10);
			fprintf(fpdata,"    %d\n", 1 + rand()%10);
		}
		printf("Generated %s with float values.\n", argv[1]);
	}
	
	fclose(fpdata);
	return 0;
}
