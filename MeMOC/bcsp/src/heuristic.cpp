/**@file generator.cpp
 * @brief main file for the Bidimensional Cutting Stock Problem greedy heuristic
 * @author Alberto De Bortoli
 */

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <time.h>

using namespace std;

/* define a new type for piece to cut */
typedef struct {
	float 	width;
	float 	length;
	int 	demand;	
} Piece;

/* define a new type for the sheet in stock to be cut */
typedef struct {
	float	width;
	float	length;
} StockSheet;

/* global data */
vector<Piece> 	pieces;
StockSheet		stockSheet;
float 			zero;


/* function for loading the data from *.dat file into main fields */
int readData(char * fname){ 
	
   FILE * fpdata = fopen(fname,"r");
   if ( fpdata == NULL ) {
       return 1;
   }
   
   int numPieces = 0;
   
   fscanf(fpdata, "param zero := %f ;\n", & zero);
   fscanf(fpdata, "param rect_width := %f ;\n", & stockSheet.width);
   fscanf(fpdata, "param rect_length := %f ;\n", & stockSheet.length);
   fscanf(fpdata, "param numPieces := %d ;\n", & numPieces);

   fscanf(fpdata,"param WIDTHS: LENGTHS: ORDERS :=\n");
   for (int i = 0; i < numPieces; i++) {
		Piece piece;
		fscanf(fpdata,"%f", & piece.width);
		fscanf(fpdata,"%f", & piece.length);
		fscanf(fpdata,"%d\n", & piece.demand);
		pieces.push_back(piece);
   }
   fscanf(fpdata,"%*s");
   fclose(fpdata);
   return 0;

}

/* aux function for sorting piece vector */
bool compare (Piece i, Piece j) {
	if (i.width == j.width)
		return (i.length > j.length);
	return (i.width > j.width);
}

/* display loaded data */
void displayLoadedData() {
	printf("Loaded data:\n");
	printf("Stock Sheet Width: %.2f\n", stockSheet.width);
	printf("Stock Sheet Length: %.2f\n", stockSheet.length);
	for (unsigned int i = 0; i < pieces.size(); i++)
		printf("Piece[%d]= [width=%.2f,\t length=%.2f,\t demand=%d]\n", i, 
			pieces[i].width, pieces[i].length, pieces[i].demand);
}

/* display the solution (patterns, strip, sheets used) */
void displaySolution(vector<vector<vector<int> > > solutions) {
	for (unsigned int i = 0; i < solutions.size(); i++){
		cout << "Sheet [" << i + 1 << "]" << endl;
		for (unsigned int j = 0; j < solutions[i].size(); j++){
			cout << "Strip [" << j + 1 << "]: [";
			unsigned int k;
			for (k = 0; k < solutions[i][j].size() - 1; k++)
				cout << " " << solutions[i][j][k] << " |";
			cout << " " << solutions[i][j][k] << " ]" << endl;
		}
		cout << endl;
	}
	cout << "Solution: " << solutions.size() << " sheets used." << endl;
}

    
int main(int args, char ** argv)
{
    /* test for loading data from *.dat file */
    if (argv[1] != NULL) {
		if (readData(argv[1]) ==1) {
			printf("ERROR reading %s.\n", argv[1]);
			exit(-1);
		}
		else printf("File %s successfully loaded.\n", argv[1]);
    }
    else {
    	printf("No argument for *.dat file.\n");
    	exit(-1);
    }
    
    /* order the pieces vector decreasingly on width and length*/
    std::sort(pieces.begin(), pieces.end(), compare);
    
    /* display the just loaded data */
	displayLoadedData();
    
    /* local data */
	float W = stockSheet.width;
	float L = stockSheet.length;
	vector<int> strip(pieces.size());
	vector<vector<int> > sheet;
	vector<vector<vector<int> > > solutions;
	
	/* clearing up vector cut */
	vector<int> cut(pieces.size());
	for (unsigned int i = 0; i < cut.size(); i++) {
		cut[i] = 0;
	}

	/* clearing up vector strip */
	for (unsigned int j = 0; j < pieces.size(); j++)
		strip[j] = 0;

	time_t start, end;
	time(&start);
	
	// test if demanded pieces fit the stock rectangles
	for (unsigned int i = 0; i < pieces.size(); i++) {
		if (!(pieces[i].width <= stockSheet.width) ) {
			cout << "stock rectangular too much small, w[i] = " << pieces[i].width << " can't fill (W = " << stockSheet.width << ")\n";
			exit(-1);
		}
	}

	/* HEURISTIC starts here */
	printf("\nSOLVING HEURISTIC\n");
	for (unsigned int i = 0; i < pieces.size(); i++) {
		// printf("\nValuto pezzo [%d]\n", i);
		// printf("Initially, cut[%d] = %d\n", i, cut[i]);
		while (cut[i] < pieces[i].demand) {	
				
			if (pieces[i].width <= W) {
				W = W - pieces[i].width;
				// considero solo i pezzi uguali o successivi a i (più piccoli)
				for (unsigned int j = i; j < pieces.size(); j++) {
					while (cut[j] < pieces[j].demand && pieces[j].length <= L) {
						L = L - pieces[j].length;
						cut[j]++;
						strip[j]++;
						// printf("cut[%d] = %d\n", j, cut[j]);
					}
				}
				/* when get here, a strip is filled */
				sheet.push_back(strip);
				L = stockSheet.length;
				
				/* clearing up vector strip */
				for (unsigned int j = 0; j < pieces.size(); j++)
					strip[j] = 0;
			}
			else {
				for (unsigned int k = i; k < pieces.size(); k++) {
					if (pieces[k].width <= W) {
						W = W - pieces[k].width;
						// considero solo i pezzi uguali o successivi a i (più piccoli)
						for (unsigned int j = k; j < pieces.size(); j++) {
							while (cut[j] < pieces[j].demand && pieces[j].length <= L) {
								L = L - pieces[j].length;
								cut[j]++;
								strip[j]++;
								// printf("cut[%d] = %d\n", j, cut[j]);
							}
						}
						/* when get here, a strip is filled */
						sheet.push_back(strip);
						L = stockSheet.length;
						
						/* clearing up vector strip */
						for (unsigned int j = 0; j < pieces.size(); j++)
							strip[j] = 0;
					}
				}
				/* save the strips in a sheet */
				solutions.push_back(sheet);
				
				/* start a new sheet */
				W = stockSheet.width;
				sheet.clear();
			}
		}
		//printf("end cutting pieces of type %d, definitely %d cut\n", i, cut[i]);	
	}
	
	time(&end);
	
	/* save the last sheet used */
	solutions.push_back(sheet);
	
	/* print out the solution */
	displaySolution(solutions);
	
	/* print out the running time */
	printf("Running time: %.2lf seconds.\n\n", difftime(end, start));
}
