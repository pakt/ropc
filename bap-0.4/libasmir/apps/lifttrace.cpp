/* A simple interface to lift traces without invoking   *
 * the OCaml frontend - ethan                           */

#include "readtrace.h"
#include <iostream>
#include <cstdlib>
#include <getopt.h>

static const string help_message =
"Usage: readtrace [options]\n"
" -t <trace_file> : read in a trace\n"
" -o <offset> : skip the first <offset> instructions (for large traces)\n"
" -a : add context info as attribute\n"
" -p : print the produced trace on stdout\n"
" -h : display this help message\n" ;

void help()
{
  cout << help_message ;
}

void Usage(char *progname)
{
  fprintf(stderr, "Invalid options for %s!\n", progname);
  help();
}

int main(int argc, char *argv[])
{
  string progname, tracename ;
  bool print = false, atts = false ;
  int offset = 0 ;


  struct option options[] = {
    {"trace", required_argument, 0, 't'},
    {"offset", required_argument, 0, 'o'},
    {"attribute", no_argument, 0, 'a'},
    {"print", no_argument, 0, 'p'},
    {"help", no_argument, 0, 'h'},
    {0,0,0,0}
  };

  if(argc < 2){
    help();
    return 0;
  }
  
  char c;
  int option_index;

  while( (c = getopt_long(argc, argv, "ahpt:o:",
			options, &option_index)) > 0 ){
    switch(c){
    case 't':
      if(optarg == NULL){
	Usage(argv[0]);
	return -1;
      }
      tracename = string(optarg);
      break;
    case 'o':
      if(optarg == NULL){
	Usage(argv[0]);
	return -1;
      }
      offset = atoi(optarg) ;
      break;
    case 'p':
      print = true ;
      break;
    case 'a':
      atts = true ;
      break;
    case 'h':
      help();
      return 0;
      break;
    default: 
      Usage(argv[0]);
      return -1;
      break;
    }
  }

  if(argc > optind) {
    progname = string(argv[optind]);
    optind++;
  }
  if(argc > optind) {
    Usage(argv[0]);
    return -1;
  }

  read_trace_from_file(tracename, offset, 0LL, print, atts, false);

  return 0;
}

