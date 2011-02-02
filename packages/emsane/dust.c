/* Try to figure out if theres dust on an ADF scanner lens.
   An adf scanner has a single pixel high, many pixel long camera element, which the paper passes over.
   if theres dust on the lens there will be an annoying long line over the entire scan.
   emsane.el tries to detect dust like this:
   - overscan the image, at the bottom of the scan there will only be dust and the default scanner background
   - cut the image in 2 halves, the lower is "dust" the upper is "image" which is treated normaly
   - "dust" is passed through dust.c
   - dust.c counts lines by using a hough transform. 

   building on a fedora(which has pkg-config support):

   gcc `pkg-config --libs --cflags opencv` dust.c -o dust

   (C) FSF 2010, Joakim Verona, GPL V3
*/
#include "cv.h"
#include "highgui.h"
#include <stdio.h>

void usage()
{
  printf("usage: dust\n");
  //  printf("filename rho theta threshold\n");
  printf("filename\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char * argv[])
{
  IplImage* input;


  if(argc!=2){ usage();};
  
  char* filename =  argv[1];
  double rho=1;//atof(argv[2]);
  double theta=CV_PI/180;//atof(argv[3]);

  int threshold=50;//atoi(argv[4]);

  //open image, convert to grayscale
  if( (input = cvLoadImage( filename, CV_LOAD_IMAGE_GRAYSCALE)) == 0 )
    return -1;
  //edge detect
  IplImage* edges;
  edges = cvCreateImage( cvGetSize(input), 8, 1 );
  cvCanny( input, edges,
           50, 200,//threshold 1 2
           3 ); //apperture
  //hugh transform
  CvSeq* lines = 0;
  CvMemStorage* storage = cvCreateMemStorage(0);
  lines=cvHoughLines2(edges, storage, CV_HOUGH_PROBABILISTIC,//CV_HOUGH_STANDARD  didnt perform well
                      rho, theta, //resolution
                      threshold,  //threshold for deciding on a line
                      50, 10); //min length, separation


  //histogram
  //find out instances of vertical lines
  //horizontal lines is really equally as bad, but it shouldnt happen and i dont want false positives
  int i;
  int vertical_lines=0;
  printf("lines:%d\n",lines->total);
  for( i = 0; i < lines->total; i++ ) 
    {
      CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
      printf("line:%d %d,%d %d,%d\n",i,line[0].x,line[0].y, line[1].x,line[1].y);
      //basically finding any lines is bad but vertical lines are really really bad
      if(line[0].x== line[1].x){
        printf("vertical!\n");
        vertical_lines++;
      }
    }
  if(vertical_lines > 0)
    exit(EXIT_FAILURE);
  printf("no vertical lines detected\n");
  return 0;
}

