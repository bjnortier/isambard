// A simple program that computes the square root of a number
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// OpenCASCADE
#include <BRep_Tool.hxx>
#include <BRepLib.hxx>
#include <BRepMesh.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <Poly_Triangulation.hxx>

// Project
#include "TutorialConfig.h"

typedef unsigned char byte;

int read_exact(byte *buf, int len) {
  int i, got=0;
  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  return(len);
}

int write_exact(byte *buf, int len) {
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
  return (len);
}

/*int read_cmd(byte *buf) {
  int len;
  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len) {
  byte li;
  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);
  return write_exact(buf, len);
}*/

int echo() {
  byte buf[100];
  while(true) {
      read_exact(buf, 1);
      cout << buf[0] << "\n";
      write_exact(buf, 1);
    }
}

/*char *buffer;
  size_t buffer_length;

  // allocate and fill the buffer

  bert_decoder_t *decoder = bert_decoder_create();
  bert_decoder_buffer(decoder, buffer, buffer_length);

  bert_data_t *data;
  int result;

  // decode BERT data
  if ((result = bert_decoder_pull(decoder, &data)) != 1) {
    fprintf(stderr,"bert error: %s\n", bert_strerror(result));
    
    bert_decoder_destroy(decoder);
    return -1;
  }
  
  if (data->type != bert_data_tuple) {
      fprintf(stderr,"BERT data was not a tuple\n");

      bert_data_destroy(data);
      bert_decoder_destroy(decoder);
      return -1;
    }

  printf("BERT tuple decoded with %d elements\n",data->tuple->length);

  bert_data_destroy(data);
  bert_decoder_destroy(decoder);*/


int main (int argc, char *argv[]) {
  echo();
  // while (read_cmd(buf) > 0) {
  //   fn = buf[0];
  //   arg = buf[1];
    
  //   if (fn == 1) {
  //     res = foo(arg);
  //   } else if (fn == 2) {
  //     res = bar(arg);
  //   }
  //   buf[0] = res;
  //   write_cmd(buf, 1);
  // }

  /*TopoDS_Shape bottle = BRepPrimAPI_MakeSphere(1.0).Shape();//MakeBottle(1.0, 1.0, 1.0);
  BRepMesh().Mesh(bottle, 0.0125);

  TopExp_Explorer Ex; 
  for (Ex.Init(bottle,TopAbs_FACE); Ex.More(); Ex.Next()) { 

    TopoDS_Face Face = TopoDS::Face(Ex.Current());

    TopLoc_Location Location = TopLoc_Location();
    
    Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);
    cout << "number of nodes: " << facing->NbTriangles() << "\n";

    for (int i = 1; i <= facing->NbTriangles(); ++i) {
      Poly_Triangle triangle = facing->Triangles().Value(i);
      Standard_Integer index1, index2, index3;
      triangle.Get(index1, index2, index3);
      //cout << index1 << ":" << index2 << ":" << index3 << "\n";

      gp_Pnt vertex1 = facing->Nodes().Value(index1);
      cout << vertex1.X() << ":" << vertex1.Y() << ":" << vertex1.Z() << "\n";
    }
    
    }*/

  return 0;
}
