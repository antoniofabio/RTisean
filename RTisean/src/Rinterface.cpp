#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <R.h>
#include <Rdefines.h>

#include "tisean21dll.h"

#define MAX_ROW_LENGTH 1000
#define MAX_ARGS_LENGTH 200
#define MAX_NAME_LENGTH 30

#include "mylog.h"

#include <string> //C++ header for my_itoa

#include <setjmp.h>
jmp_buf my_exit_point;

//needed to generate process dependent names for temporary files
int process_id;//initialized only once during package loading (value passed from R)


               
extern "C"{

void call_TISEAN_clean(double* d, int* len, char** args, char** routine);//args does not contain the input filename and -o output filename 
void call_TISEAN_bare(char** args, char** routine);//*args is the FULL options string 
void delete_suffix(char** suffix);//deletes outputfile<suffix> and inputfile if these exist
void delete_file(char** filename);
void output_format_extended(int* s, int* e, int* t, int* c, const int* limit );//gets format info on outputfile
void output_format_extended2(int* s, int* e, int* t, int* c, const int* limit, char** suffix);//gets format info on outputfile<suffix>
void output_format_extended2_new(int* s, int* e, int* t, int* c, const int* limit, char** suffix);//gets format info on outputfile<suffix>
SEXP read_TISEAN_output_extended(SEXP s, SEXP e, SEXP t, SEXP c);//reads result from outputfile given info on format and deletes outputfile
SEXP read_TISEAN_output_extended2(SEXP s, SEXP e, SEXP t, SEXP c, SEXP suffix);//reads result from outputfile<suffix> given info on format and deletes outputfile<suffix>
void write_array_to_file(double* d, int* len, int* p, int* q, int* ioflag, char** filenameptr);//writes R numeric vector to *filenameptr as a (*p)*(*q) matrix 
									//according to R conventions (i.e. the vector is column1,column2,...)
									//ioflag=0 to rewrite, not 0 to append
void write_array_to_temp(double* d, int* len, int* p, int* q, int* ioflag);//writes an array to inputfile
void write_to_temp(double* d, int* len);//writes an array to tiseanin$$$ as a single column
void write_string_to_file(char** strptr, int* ioflag, char** filenameptr);//writes *str to *filenameptr
void write_string_to_file2(char** strptr, int* ioflag, char** filenameptr);//writes *str to *filenameptr and appends \n (newline)
void set_process_id(int* id);
void Rinput_filename(char** str);//returns the string tin$<pid>
void Routput_filename(char** str);//returns the string tout$<pid>
void Rparam_filename(char** str,int* ordinal);//returns the string tparam$<*ordinal><pid> (used for additional parameter filenames)				    
void Rlog_filename(char** str);
}


static void input_filename(char** stra);//returns the string tin$<pid>
static void output_filename(char** stra);//returns the string tout$<pid>
static void param_filename(char** stra,int* ordinal);//returns the string tparam$<*ordinal><pid> (used for additional parameter filenames)
static char* my_itoa( int value, char* result, int base );
static void log_filename(char** stra);
static SEXP read_result_extended(SEXP s, SEXP e, SEXP t, SEXP c);
static SEXP read_result_extended2(SEXP s, SEXP e, SEXP t, SEXP c, SEXP file_extension);
static void adapt_arguments(double* d, int len, char* s);
static SEXP read_from_file_extended(int* s, int* e, int* t, int* c, char* filename);
static int number_of_lines(char* filename);
static int write_to_file(double* d, int len, char* filename);
static void adapt_arguments(double* d, int len, char* s);

//copied from http://www.jb.man.ac.uk/~slowe/cpp/itoa.html
static char* my_itoa( int value, char* result, int base ) {
        // check that the base if valid
        
        if (base < 2 || base > 16) { *result = 0; return result; }        
        char* out = result;
        int quotient = value;
        do {
                *out = "0123456789abcdef"[ std::abs( quotient % base ) ];
                ++out;
                quotient /= base;
        } while ( quotient );
                
        // Only apply negative sign for base 10
        
        if ( value < 0 && base == 10) *out++ = '-';        
        std::reverse( result, out );
        *out = 0;
        return result;
}



static int my_fcloseall(){
#ifdef _WIN32
	return _fcloseall();
#endif
#ifndef _WIN32
	return fcloseall();
#endif
}

extern "C"{
void set_process_id(int* id){
    process_id=*id;
}
void Rinput_filename(char** str){
	*str=R_alloc(MAX_NAME_LENGTH,1);
	input_filename(str);
}
void Routput_filename(char** str){
	*str=R_alloc(MAX_NAME_LENGTH,1);
	output_filename(str);	
}
void Rparam_filename(char** str,int* ordinal){
	*str=R_alloc(MAX_NAME_LENGTH,1);
	param_filename(str,ordinal);
}			    
void Rlog_filename(char** str){
	*str=R_alloc(MAX_NAME_LENGTH,1);
	log_filename(str);
}

}


// assumes that enough space is allocated for *str
static void input_filename(char** stra){
        char* str = *stra;
	char* init="tin$";
	strcpy(str,init);
	int id = process_id;
	char buffer[MAX_NAME_LENGTH];
	my_itoa(id,buffer,16);
	strcpy(str+strlen(init),buffer);
	*(str+strlen(init)+strlen(buffer))='\0';
}

// assumes that enough space is allocated for *str
static void output_filename(char** stra){
        char* str = *stra;
	char* init="tout$";
	strcpy(str,init);
	int id = process_id;
	char buffer[MAX_NAME_LENGTH];
	my_itoa(id,buffer,16);
	strcpy(str+strlen(init),buffer);
	*(str+strlen(init)+strlen(buffer))='\0';
}

// assumes that enough space is allocated for *str
static void param_filename(char** stra,int* ordinal){
        char* str = *stra;
	char* init="tparam$";
	strcpy(str,init);
	int id = process_id;
	char buffer[MAX_NAME_LENGTH];
        char buffer1[MAX_NAME_LENGTH];
	my_itoa(id,buffer,16);
        my_itoa(*ordinal,buffer1,10);
	strcpy(str+strlen(init),buffer);
        strcpy(str+strlen(init)+strlen(buffer),buffer1);
	*(str+strlen(init)+strlen(buffer)+strlen(buffer1))='\0';
}

// assumes that enough space is allocated for *str
static void log_filename(char** stra){
        char* str = *stra;
	char* init="tlog$";
	strcpy(str,init);
	int id = process_id;
	char buffer[MAX_NAME_LENGTH];
	my_itoa(id,buffer,16);
	strcpy(str+strlen(init),buffer);
	*(str+strlen(init)+strlen(buffer))='\0';
}




void mylog(double *d, int len){
	int i;
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
        FILE* f=fopen(tlog,"a");
	
	for (i = 0; i < len; i++){
            fprintf(f,"%21.16e\n",d[i]);
	}
	fclose(f);
	
}

void mylog(char* s){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
        FILE* f=fopen(tlog,"a");
        fprintf(f,"%s\n",s);
	fclose(f);
	
}


void mylog(int *d, int len){
	int i;
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	
	for (i = 0; i < len; i++){
            fprintf(f,"%d\n",d[i]);
	}
	fclose(f);
	
}

void mylog(int d){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	
        fprintf(f,"%d\n",d);
	
	fclose(f);
	
}

void mylog(unsigned int d){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	
            fprintf(f,"%d\n",d);
	
	fclose(f);
	
}

void mylog(long d){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	 fprintf(f,"%d\n",d);
	
	fclose(f);
	
}


void mylog(unsigned long d){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	fprintf(f,"%d\n",d);
	
	fclose(f);
	
}


void mylog(double d){
	char buffer[MAX_NAME_LENGTH];
	char* tlog=buffer;
	log_filename(&tlog);
	FILE* f=fopen(tlog,"a");        
	fprintf(f,"%21.16e\n",d);
	fclose(f);
	
}


extern "C" {

//routine which actually calls the wrapper functions, accepts an array of doubles,
//a string of command line options, and a string with the name of a TISEAN routine and returns
//the number of lines of the output, after it creates the corresponding output file.

//assumes that the input data is already in temporary input file and that *args is the full
//options string, containing the input and output file names and the -V0 option
void call_TISEAN_bare(char** args, char** routine){
	int i,j;
	char* s = *args;
	
	if (setjmp(my_exit_point)!=0){
	    //optionally write a one-byte file which can be used to detect abnormal exit
	    return;
	}
	
	if (strcmp(*routine,"ar-model")==0){
		ar_model(&i,&j,&s);
	}
	if (strcmp(*routine,"av-d2")==0){
		av_d2(&i,&j,&s);
	}
        if (strcmp(*routine,"boxcount")==0){
		boxcount(&i,&j,&s);
	}
	if (strcmp(*routine,"corr")==0){
		corr(&i,&j,&s);
	}
	if (strcmp(*routine,"d2")==0){
		d2(&i,&j,&s);
	}
	if (strcmp(*routine,"extrema")==0){
		extrema(&i,&j,&s);
	}
	if (strcmp(*routine,"false_nearest")==0){
		false_nearest(&i,&j,&s);
	}
	if (strcmp(*routine,"fsle")==0){
		fsle(&i,&j,&s);
	}
	if (strcmp(*routine,"ghkss")==0){
		ghkss(&i,&j,&s);
	}
	if (strcmp(*routine,"histogram")==0){
		histogram(&i,&j,&s);
	}
	if (strcmp(*routine,"ll-ar")==0){
		ll_ar(&i,&j,&s);
	}
	if (strcmp(*routine,"low121")==0){
		low121(&i,&j,&s);
	}
	if (strcmp(*routine,"lyap_k")==0){
		lyap_k(&i,&j,&s);
	}
	if (strcmp(*routine,"lyap_r")==0){
		lyap_r(&i,&j,&s);
	}
	if (strcmp(*routine,"lyap_spec")==0){
		lyap_spec(&i,&j,&s);
	}
	if (strcmp(*routine,"makenoise")==0){
		makenoise(&i,&j,&s);
	}
	if (strcmp(*routine,"mem_spec")==0){
		mem_spec(&i,&j,&s);
	}
	if (strcmp(*routine,"mutual")==0){
		mutual(&i,&j,&s);
	}
	if (strcmp(*routine,"nrlazy")==0){
		nrlazy(&i,&j,&s);
	}
	if (strcmp(*routine,"nstat_z")==0){
		nstat_z(&i,&j,&s);
	}
	if (strcmp(*routine,"nstep")==0){
		nstep(&i,&j,&s);
	}
	if (strcmp(*routine,"onestep")==0){
		onestep(&i,&j,&s);
	}
	if (strcmp(*routine,"poincare")==0){
		poincare(&i,&j,&s);
	}
	if (strcmp(*routine,"polyback")==0){
		polyback(&i,&j,&s);
	}
	if (strcmp(*routine,"polynom")==0){
		polynom(&i,&j,&s);
	}
	if (strcmp(*routine,"polynomp")==0){
		polynomp(&i,&j,&s);
	}
	if (strcmp(*routine,"polypar")==0){
		polypar(&i,&j,&s);
	}
	if (strcmp(*routine,"rbf")==0){
		rbf(&i,&j,&s);
	}
	if (strcmp(*routine,"recurr")==0){
		recurr(&i,&j,&s);
	}
	if (strcmp(*routine,"resample")==0){
		resample(&i,&j,&s);
	}
	if (strcmp(*routine,"rescale")==0){
		rescale(&i,&j,&s);
	}
	if (strcmp(*routine,"sav_gol")==0){
		sav_gol(&i,&j,&s);
	}
	if (strcmp(*routine,"svd")==0){
		svd(&i,&j,&s);
	}
	if (strcmp(*routine,"xcor")==0){
		xcor(&i,&j,&s);
	}
	if (strcmp(*routine,"xzero")==0){
		xzero(&i,&j,&s);
	}
	if (strcmp(*routine,"zeroth")==0){
		zeroth(&i,&j,&s);
	}	
} 



SEXP read_TISEAN_output_extended(SEXP s, SEXP e, SEXP t, SEXP c){
	return read_result_extended(s,e,t,c);
}


SEXP read_TISEAN_output_extended2(SEXP s, SEXP e, SEXP t, SEXP c, SEXP suffix){
	return read_result_extended2(s,e,t,c,suffix);
}

void delete_suffix(char** suffix){
	char bufferin[MAX_NAME_LENGTH];
	char bufferout1[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout1=bufferout1;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout1);

	char* suffixp= *suffix;
	int lsuf=strlen(*suffix);	
	strcpy(tout,tout1);
	strcpy(tout+strlen(tout1),suffixp);
	tout[strlen(tout1)+lsuf]='\0';	
	
	my_fcloseall();
	remove(tin);
	remove(tout);

}

void delete_file(char** filename){
	FILE* f = fopen(*filename,"r");
	if (f != NULL){
		fclose(f);
		remove(*filename);
	}	
}


}//here extern "C"{} ends


//routine for writing an array of doubles into a file delimited by linefeeds
//checks only if it is possible to open file for writing
static int write_to_file(double* d, int len, char* filename){
	FILE* f = fopen(filename,"w");
	if (f == NULL) return 1;
	int i;
	for (i = 0; i < len; i++){
		fprintf(f,"%21.16e\n",d[i]);
	}
	fclose(f);
	return 0;
}

extern "C"{
void write_to_temp(double* d, int* len){
	char bufferin[MAX_NAME_LENGTH];
	char* tin=bufferin;
	input_filename(&tin);
	write_to_file(d,*len,tin);
}

void write_string_to_file(char** strptr, int* ioflag, char** filenameptr){
	char* tin=*filenameptr;
	char* modifier;
	char* am="a";
	char* wm="w";
	if ((*ioflag)==0)
		modifier=wm;
	else
		modifier=am;
	FILE* f = fopen(tin,modifier);
	fprintf(f,"%s",*strptr);	
	fclose(f);
}


void write_string_to_file2(char** strptr, int* ioflag, char** filenameptr){
	char* buffer=(char*) malloc(sizeof(char)*strlen(*strptr)+10);
	sprintf (buffer, "%s\n", *strptr);//we rely on being the resulting buffer \0 terminated
	char* tin=*filenameptr;
	char* modifier;
	char* am="a";
	char* wm="w";
	if ((*ioflag)==0)
		modifier=wm;
	else
		modifier=am;
	FILE* f = fopen(tin,modifier);
	fprintf(f,"%s",buffer);	
	fclose(f);
}

}



//low level routine to write an R vector (e.g. as.double(c(1,2,3..)) 
//as a (*p)*(*q) matrix (using R convention of column1, column2,...) to file named *filenameptr
extern "C"{
void write_array_to_file(double* d, int* len, int* p, int* q, int* ioflag, char** filenameptr){
	char* tin=*filenameptr;
	char* modifier;
	char* am="a";
	char* wm="w";
	if ((*ioflag)==0)
		modifier=wm;
	else
		modifier=am;
	if ((*p)*(*q)!=*len)
		return;
	FILE* f = fopen(tin,modifier);
	if (f == NULL) return;
	int i,j;
	for (i = 0; i < *p; i++){
		for (j = 0; j < *q; j++){
			fprintf(f,"%21.16e ",d[i+(*p)*j]);
		}
		fprintf(f,"%s","\n");
	}
	fclose(f);
}

void write_array_to_temp(double* d, int* len, int* p, int* q, int* ioflag){
	char bufferin[MAX_NAME_LENGTH];
	char* tin=bufferin;
	input_filename(&tin);
	char* modifier;
	char* am="a";
	char* wm="w";
	if ((*ioflag)==0)
		modifier=wm;
	else
		modifier=am;
	if ((*p)*(*q)!=*len)
		return;
	FILE* f = fopen(tin,modifier);
	if (f == NULL) return;
	int i,j;
	for (i = 0; i < *p; i++){
		for (j = 0; j < *q; j++){
			fprintf(f,"%21.16e ",d[i+(*p)*j]);
		}
		fprintf(f,"%s","\n");
	}
	fclose(f);
}

}



static int number_of_words(char* s){
	char* s1 = (char*) malloc( (strlen(s)+1) * sizeof(char) );
	strcpy(s1,s);
	s1[strlen(s)]='\0';
	char *sub_string;
	int i = 0;
					/* Extract first string	*/
	sub_string = strtok(s1, " \n");
	if (sub_string == NULL){ 
		free(s1);	
		return i;
	}
	else
		i++;
  	while ( (sub_string=strtok(NULL, " \n")) != NULL){
    		i++;
  	}
	free(s1);
	return i;
}






static int is_numeric(char* str){
	char* str1;
	str1 = strtok(str," \n");
	if (!isdigit(str1[0]) && !(str1[0] == '+') && !(str1[0] == '-'))
		return 0;
	else
		return 1;
}


static void format_extended(char* filename, int* s, int* e, int* t, int* c, const int* limit){
	
// the function returns the start line numbers, the end line numbers and the "types" of the blocks
// in the file. "Type" is 0 if the first word of the block begins with a nondigit character which is
// not '+' or '-'; otherwise it is the number of words in the first line of the block.

// the array arguments s,e,t sh should be of length equal to limit 
	
	
	FILE *f = fopen(filename,"r");
	if (f == NULL) {
		*c=0;	
		return;
	}
	
	int i=0;
	int count=0;
	char* str = (char*) malloc(sizeof(char)*MAX_ROW_LENGTH);
	char* str1;
	int n;
	int startfound=0;
	int linetype=0; //0 initial, 1 string, 2 digit 

	while (fgets(str,MAX_ROW_LENGTH,f) != NULL && count<(*limit)){

			i++;
			n = number_of_words(str);
			if (n>0){
				//get the first word
				
				if (startfound == 0){
					s[count]=i;
					startfound=1;
					if (is_numeric(str))
						t[count]=n;
					else
						t[count]=0;
				}
				
				
				if (!is_numeric(str)){
					if (linetype==2 && startfound==1){
						e[count]=i-1;
						count++;
						s[count]=i;
						t[count]=0;//means the type here a string
					}
					linetype=1;
				}
				else{
					if (linetype==1 && startfound==1){
						e[count]=i-1;
						count++;
						s[count]=i;
						t[count]=n;
					}
					linetype=2;	
				}
				
			}
			else{
				if (startfound==1){
					e[count]=i-1;
					startfound=0;
					count++;
					linetype=0;
				}
			}
		
	}
	if (startfound == 1){
		e[count]=i;
		startfound=0;
		count++;			
	}
	*c=count;
	free(str);
	fclose(f);
}


static void format_extended_new(char* filename, int* s, int* e, int* t, int* c, const int* limit){
// checks every line for an argument type and a number of words
// the function returns the start line numbers, the end line numbers and the "types" of the blocks
// in the file. "Type" is 0 if the first word of the block begins with a nondigit character which is
// not '+' or '-'; otherwise it is the number of words in the first line of the block.

// the array arguments s,e,t sh should be of length equal to limit 
	FILE *f = fopen(filename,"r");
	if (f == NULL) {
		*c=0;	
		return;
	}
	
	int i=0;
	int count=0;
	char* str = (char*) malloc(sizeof(char)*MAX_ROW_LENGTH);
	char* str1;
	int n;
	int startfound=0;
	int linetype=0; //0 initial, 1 string, 2 digit 

	while (fgets(str,MAX_ROW_LENGTH,f) != NULL && count<(*limit)){

			i++;
			n = number_of_words(str);
			if (n>0){
				//get the first word
				
				if (startfound == 0){
					s[count]=i;
					startfound=1;
					if (is_numeric(str)){
						t[count]=n;
						linetype=2;
					}
					else{
						t[count]=0;
						linetype=1;
					}
				}
				else{				
				    if (!is_numeric(str)){
					    if (linetype==2){
						    e[count]=i-1;
						    count++;
						    s[count]=i;
						    t[count]=0;//means the type here a string
						    startfound=1;
						    linetype=1;
					    }
				    }
				    else{
					    if (linetype==1 || linetype==2 && n!=t[count]){
						    e[count]=i-1;
						    count++;
						    s[count]=i;
						    t[count]=n;
						    startfound=1;
						    linetype=2;
					    }
				    }
				}
				
			}
			else{
				if (startfound==1){
					e[count]=i-1;
					startfound=0;
					count++;
					linetype=0;
				}
			}
		
	}
	if (startfound == 1){
		e[count]=i;
		startfound=0;
		count++;			
	}
	*c=count;
	free(str);
	fclose(f);
}

extern "C"{
void output_format_extended(int* s, int* e, int* t, int* c, const int* limit){
	//to avoid the situation that tin,tout are still open - this might 
	//produce erroneous results and leave temporary files not removed
	char bufferout[MAX_NAME_LENGTH];
	char* tout=bufferout;
	output_filename(&tout);
	my_fcloseall();
	format_extended(tout, s, e, t, c, limit);
}
}


extern "C"{
void output_format_extended2(int* s, int* e, int* t, int* c, const int* limit, char** suffix){
	//to avoid the situation that tin,tout are still open - this might 
	//produce erroneous results and leave temporary files not removed
	char bufferin[MAX_NAME_LENGTH];
	char bufferout1[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout1=bufferout1;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout1);
	

	my_fcloseall();
	strcpy(tout,tout1);
	strcpy(tout+strlen(tout1),*suffix);

	tout[strlen(tout1)+strlen(*suffix)]='\0';
	format_extended(tout, s, e, t, c, limit);
}

void output_format_extended2_new(int* s, int* e, int* t, int* c, const int* limit, char** suffix){
	//to avoid the situation that tin,tout are still open - this might 
	//produce erroneous results and leave temporary files not removed
	char bufferin[MAX_NAME_LENGTH];
	char bufferout1[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout1=bufferout1;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout1);
	

	my_fcloseall();
	strcpy(tout,tout1);
	strcpy(tout+strlen(tout1),*suffix);

	tout[strlen(tout1)+strlen(*suffix)]='\0';
	format_extended_new(tout, s, e, t, c, limit);
}

}


static SEXP read_from_file_extended(int* s, int* e, int* t, int* c, char* filename){
	
	SEXP* pstrs;
	SEXP* perror_SEXP;
	double* pd;
	
	SEXP list,strs,d;
	SEXP error_SEXP;
	
	PROTECT(error_SEXP = NEW_CHARACTER(1));
	perror_SEXP = CHARACTER_POINTER(error_SEXP);
	perror_SEXP[0] = mkChar("IO_ERROR");	

	FILE *f = fopen(filename,"r");
	UNPROTECT(1);
	if (f == NULL)
		return error_SEXP;
	int i=1;//line number
	char* str;//s[MAX_ROW_LENGTH];
	str = (char*) malloc(sizeof(char)*MAX_ROW_LENGTH);
	char* st = str;
	char* sub_string;
	char* stop;

	int count=*c;
	//we expect as an answer a list containing (count) objects, each of which is either an array of strings,
	//or an array of doubles, which we later convert in R into matrices 
	PROTECT(list = allocVector(VECSXP,count));

	for (int m=0; m<count; m++){

		//skip to the beginning of the block
		while (i<s[m]){
			fgets(str,MAX_ROW_LENGTH,f);
			i++;
		}

		if (t[m]==0){
			PROTECT(strs = NEW_CHARACTER(e[m]-s[m]+1));
			pstrs = CHARACTER_POINTER(strs);
			for (i = s[m]; i <= e[m]; i++){
				fgets(str,MAX_ROW_LENGTH,f);
				pstrs[i-s[m]] = mkChar(str);
			}
			SET_VECTOR_ELT(list,m,strs);
		}
		else{
			PROTECT(d = NEW_NUMERIC((e[m]-s[m]+1)*t[m]));
			pd = NUMERIC_POINTER(d);
			for (i = s[m]; i <= e[m]; i++){
				fgets(str,MAX_ROW_LENGTH,f);
				sub_string = strtok(str, " \n");
				pd[i-s[m]] = strtod(sub_string,&stop);
				int j = 0;
					/* Extract remaining 
					 * strings 		*/
  				while ( (sub_string=strtok(NULL, " \n")) != NULL && j<t[m]){
    					j++;
					pd[i-s[m]+j*(e[m]-s[m]+1)] = strtod(sub_string,&stop);
					
  				}	
			}
			SET_VECTOR_ELT(list,m,d);
		}
	}

	free(st);
	fclose(f);
	
	UNPROTECT(count+1);
	
	return list;

}



//writes the array d into the temporary input file and updates the TISEAN option string s by inserting the 
//input filename into the beginning of the string s and " -o <output file_name>" into its end.
static void adapt_arguments(double* d, int len, char* s){
	char bufferin[MAX_NAME_LENGTH];
	char bufferout1[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout1=bufferout1;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout1);
	char t[MAX_ARGS_LENGTH];
	char* op=" -o ";
        char* space = " ";
        strcpy(tout,op);
	strcpy(tout+strlen(op),tout1);
	strcpy(tout+strlen(op)+strlen(tout1),space);
	*(tout+strlen(op)+strlen(tout1)+strlen(space))='\0';
	

	strcpy(t,s);
	t[strlen(s)]='\0';
	strcpy(s,tin);
	strcpy(s+strlen(tin),space);
	strcpy(s+strlen(tin)+strlen(space),t);
	strcpy(s+strlen(t)+strlen(tin)+strlen(space),tout);
	s[strlen(t)+strlen(tin)+strlen(space)+strlen(tout)]='\0';
	write_to_file(d,len,tin);
}



static SEXP read_result_extended(SEXP sp, SEXP ep, SEXP tp, SEXP cp){
	char bufferin[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout);
        

	int c;
	c = INTEGER_VALUE(cp);

	int* s = (int*) malloc(c*sizeof(int));
	int* e = (int*) malloc(c*sizeof(int));
	int* t = (int*) malloc(c*sizeof(int));
	
	for (int h=0; h<c; h++){
		s[h]= INTEGER(sp)[h];
		e[h]= INTEGER(ep)[h];
		t[h]= INTEGER(tp)[h];
	}
	
	//to avoid the situation that tin,tout are still open - this might 
	//produce erroneous results and leave temporary files not removed
	my_fcloseall();
	SEXP res = read_from_file_extended(s,e,t,&c,tout);
	
	
        free(t);
	free(e);
	free(s);

	remove(tout);
		
	return res;
}

static SEXP read_result_extended2(SEXP sp, SEXP ep, SEXP tp, SEXP cp, SEXP suffixp){
	char bufferin[MAX_NAME_LENGTH];
	char bufferout1[MAX_NAME_LENGTH];
	char bufferout[MAX_NAME_LENGTH];
	char* tin=bufferin;
	char* tout1=bufferout1;
	char* tout=bufferout;
	input_filename(&tin);
	output_filename(&tout1);

	char* Psuffixp;
	PROTECT(suffixp = AS_CHARACTER(suffixp));
	int lsuf = strlen(CHAR(STRING_ELT(suffixp,0)));
	Psuffixp = (char*) malloc((lsuf+1)*sizeof(char));
	strcpy(Psuffixp,CHAR(STRING_ELT(suffixp,0)));
	Psuffixp[strlen(CHAR(STRING_ELT(suffixp,0)))]='\0';
	UNPROTECT(1);
		
	strcpy(tout,tout1);
	strcpy(tout+strlen(tout1),Psuffixp);
	tout[strlen(tout1)+lsuf]='\0';	
	

	int c;
	c = INTEGER_VALUE(cp);

	int* s = (int*) malloc(c*sizeof(int));
	int* e = (int*) malloc(c*sizeof(int));
	int* t = (int*) malloc(c*sizeof(int));
	
	for (int h=0; h<c; h++){
		s[h]= INTEGER(sp)[h];
		e[h]= INTEGER(ep)[h];
		t[h]= INTEGER(tp)[h];
	}
	
	//to avoid the situation that tin,tout are still open - this might 
	//produce erroneous results and leave temporary files not removed
	my_fcloseall();	
	SEXP res = read_from_file_extended(s,e,t,&c,tout);

	remove(tout);

	free(t);
	free(e);
	free(s);
	free(Psuffixp);

	return res;
}



static void print_array(double *d, int len){
	int i;
	printf("\n");
	for (i = 0; i < len; i++){
		printf("%21.16e\n",d[i]);
	}
	printf("\n");
}

