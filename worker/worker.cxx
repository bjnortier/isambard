// A simple program that computes the square root of a number
#include <stdio.h>
#include <iostream>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>


// OpenCASCADE
/*#include <BRep_Tool.hxx>
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
#include <Poly_Triangulation.hxx>*/

// Spirit
#include "json_spirit.h"

// Project
#include "WorkerConfig.h"

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

int write_exact(unsigned char *buf, int len) {
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
  return (len);
}


int main (int argc, char *argv[]) {

  while(true) {
    char buf[1024*1024];
    size_t nbytes;
    ssize_t bytes_read;
    nbytes = sizeof(buf);
    bytes_read = read(0, buf, nbytes);

    if (bytes_read > 0) {
        
      json_spirit::Value value;
      // Set 0-delimiter for conversion to std::string
      buf[bytes_read] = 0;
      std::string string = std::string(buf);
      json_spirit::read(string, value);
        
      json_spirit::mArray addr_array;
      json_spirit::mObject addr_obj;
        
      addr_obj[ "type" ] = "cuboid";
      addr_obj[ "width" ] = 1.0;
        
      addr_array.push_back( addr_obj );
        
      std::string output = json_spirit::write(addr_array);
      write(1, output.c_str(), output.size());
        
    }
  }


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
